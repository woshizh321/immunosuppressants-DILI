suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(glmnet)
  library(stringr)
})

# ============================================================
# 0) Inputs
# ============================================================
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target <- "Hepatobiliary disorders"

# project assets
source("99_shared/dict/immunosuppressants_drug_classes.R")

# output
out_dir <- "04_lasso/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# duckdb spill-safe
duckdb_dir  <- "99_shared/tmp_duckdb"
dir.create(duckdb_dir, recursive = TRUE, showWarnings = FALSE)
duckdb_file <- file.path(duckdb_dir, "lasso_dili_work.duckdb")

# run label
run_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ============================================================
# 1) QC-first: check required columns exist
# ============================================================
con <- dbConnect(duckdb(), dbdir = duckdb_file)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")
dbExecute(con, sprintf("PRAGMA temp_directory='%s';",
                       normalizePath(duckdb_dir, winslash = "/", mustWork = FALSE)))
dbExecute(con, "PRAGMA enable_object_cache=true;")

# Read schema (DuckDB can DESCRIBE a parquet scan)
schema <- dbGetQuery(con, sprintf("
  DESCRIBE SELECT * FROM read_parquet('%s') LIMIT 1
", faers_path))

need_cols <- c("caseid", "drugname", "role_cod", "soc_name", "sex_std", "age_group")
miss <- setdiff(need_cols, schema$column_name)
if (length(miss) > 0) {
  stop("FAERS master missing required columns: ", paste(miss, collapse = ", "))
}

cat("QC passed: required columns exist.\n")

# ============================================================
# 2) Materialize slim tables (one-time)
# ============================================================

# 2.1 total distinct cases
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE total_cases AS
  SELECT COUNT(DISTINCT caseid)::BIGINT AS n_total
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL;
", faers_path))
n_total <- dbGetQuery(con, "SELECT n_total FROM total_cases;")$n_total[1]
cat("Total distinct cases:", n_total, "\n")

# 2.2 case-level demographics (sex_std, age_group)
# Use MAX/ANY_VALUE-like selection: choose first non-null per case if multiple rows exist.
# DuckDB supports arg_max, but simplest is: aggregate with MAX on strings is OK if consistent.
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_demo AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(sex_std)  AS sex_std,
    MAX(age_group) AS age_group
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid;
", faers_path))

# 2.3 DILI outcome: SOC flag at case level
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_dili AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(CASE WHEN soc_name = '%s' THEN 1 ELSE 0 END) AS DILI
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid;
", soc_target, faers_path))

# 2.4 PS/SS drug rows restricted to immunosuppressant universe (huge reduction)
all_is_drugs <- unique(toupper(unlist(drug_classes)))
dbWriteTable(con, "all_is_drug_list",
             data.frame(drugname = all_is_drugs, stringsAsFactors = FALSE),
             overwrite = TRUE)

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE psss_is_rows AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname))   AS drugname
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND role_cod IN ('PS','SS')
    AND UPPER(TRIM(drugname)) IN (SELECT drugname FROM all_is_drug_list);
", faers_path))

# ============================================================
# 3) Build case-level exposures by 5 drug classes (0/1 each)
# ============================================================
# We'll create one temp table per class and then left-join into final feature table.

make_class_flag <- function(cls_name, drug_vec) {
  dbWriteTable(con, "drug_list_tmp",
               data.frame(drugname = unique(toupper(drug_vec)), stringsAsFactors = FALSE),
               overwrite = TRUE)
  flag_tbl <- paste0("flag_", str_replace_all(tolower(cls_name), "[^a-z0-9]+", "_"))
  sql <- sprintf("
    CREATE OR REPLACE TEMP TABLE %s AS
    SELECT
      caseid,
      1 AS %s
    FROM psss_is_rows
    WHERE drugname IN (SELECT drugname FROM drug_list_tmp)
    GROUP BY caseid;
  ", flag_tbl, flag_tbl)
  dbExecute(con, sql)
  flag_tbl
}

flag_tables <- c()
for (cls in names(drug_classes)) {
  flag_tables <- c(flag_tables, make_class_flag(cls, drug_classes[[cls]]))
}

cat("Built exposure flags for classes:\n")
print(flag_tables)

# ============================================================
# 4) Assemble final modeling table (one row per case)
# ============================================================
# Start from case_dili (all cases), join demo + flags.
# Missing flags -> 0.

join_flags_sql <- paste(
  vapply(flag_tables, function(tn) {
    sprintf("LEFT JOIN %s f_%s ON b.caseid = f_%s.caseid", tn, tn, tn)
  }, character(1)),
  collapse = "\n"
)

select_flags_sql <- paste(
  vapply(flag_tables, function(tn) {
    sprintf("COALESCE(f_%s.%s, 0) AS %s", tn, tn, tn)
  }, character(1)),
  collapse = ",\n    "
)

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE model_case_table AS
  WITH base AS (
    SELECT d.caseid, d.DILI
    FROM case_dili d
  )
  SELECT
    b.caseid,
    b.DILI,
    cd.sex_std,
    cd.age_group,
    %s
  FROM base b
  LEFT JOIN case_demo cd ON b.caseid = cd.caseid
  %s
", select_flags_sql, join_flags_sql))

# Quick QC summary
qc <- dbGetQuery(con, "
  SELECT
    COUNT(*) AS n_cases,
    SUM(DILI) AS n_dili,
    ROUND(1.0 * SUM(DILI)/COUNT(*), 6) AS dili_rate
  FROM model_case_table
")
print(qc)

# Pull to R (12 predictors only -> manageable)
dat <- dbGetQuery(con, "SELECT * FROM model_case_table")

# ============================================================
# 5) QC in R: standardize sex/age categories and one-hot encode
# ============================================================

# 5.1 normalize sex: expected sexMale/sexFemale/sexOther
dat <- dat %>%
  mutate(
    sex_std = as.character(sex_std),
    sex_std = ifelse(is.na(sex_std) | sex_std == "" | sex_std %in% c("Unknown","UNK","U"), "Other", sex_std),
    sex_std = ifelse(sex_std %in% c("M","Male"), "Male", sex_std),
    sex_std = ifelse(sex_std %in% c("F","Female"), "Female", sex_std),
    sex_std = ifelse(!sex_std %in% c("Male","Female"), "Other", sex_std)
  )

# 5.2 normalize age_group into 4 bins: <18, 18-39, 40-64, 65+
# If your age_group already equals these strings, this is a no-op.
dat <- dat %>%
  mutate(
    age_group = as.character(age_group),
    age_group = ifelse(is.na(age_group) | age_group == "" | age_group %in% c("Unknown","UNK","U"), NA, age_group)
  )

# Map common labels to target bins (adjust if your age_group labels differ)
map_age <- function(x){
  if (is.na(x)) return(NA_character_)
  x2 <- str_replace_all(x, "\\s+", "")
  x2 <- str_replace_all(x2, "_", "-")
  if (x2 %in% c("<18","0-17","0-18","<18years","<18y")) return("<18")
  if (x2 %in% c("18-39","18-40","18-39years","18-39y")) return("18-39")
  if (x2 %in% c("40-64","40-65","40-64years","40-64y")) return("40-64")
  if (x2 %in% c("65+",">=65","65plus","65-","65以上")) return("65+")
  # if already clean:
  if (x %in% c("<18","18-39","40-64","65+")) return(x)
  return(NA_character_)
}

dat$age_bin <- vapply(dat$age_group, map_age, character(1))
# If you have many NAs here, we can re-map once you paste your age_group unique values.

# Keep only rows with known sex + age_bin (QC-first; you can choose to keep unknowns later)
dat_model <- dat %>%
  filter(!is.na(age_bin))

# 5.3 Create design matrix
# Build formula using:
# - sex_std (3 levels)
# - age_bin (4 levels)
# - 5 class flags (already 0/1)
flag_cols <- flag_tables
# ensure flags are numeric 0/1
dat_model[flag_cols] <- lapply(dat_model[flag_cols], function(x) as.numeric(x))

# Use model.matrix for categorical vars
x <- model.matrix(
  ~ sex_std + age_bin + .,
  data = dat_model %>% select(sex_std, age_bin, all_of(flag_cols))
)

# Remove intercept (glmnet adds it)
x <- x[, colnames(x) != "(Intercept)", drop = FALSE]

y <- as.integer(dat_model$DILI)

cat("Model matrix dim:", dim(x)[1], "x", dim(x)[2], "\n")
cat("Positive (DILI=1):", sum(y==1), " / ", length(y), "\n")

# ============================================================
# 6) Logistic LASSO with CV
# ============================================================

set.seed(20251218)

# optional: class weights to handle imbalance (recommended)
# weight positives higher so model doesn't trivialize
p <- mean(y==1)
w <- ifelse(y==1, 0.5/p, 0.5/(1-p))

cvfit <- cv.glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = 1,
  nfolds = 10,
  type.measure = "deviance",
  weights = w
)

# choose lambda (both exported)
lambda_min <- cvfit$lambda.min
lambda_1se <- cvfit$lambda.1se

cat("lambda.min:", lambda_min, "\n")
cat("lambda.1se:", lambda_1se, "\n")

# ============================================================
# 7) Export coefficients (OR) at lambda.min and lambda.1se
# ============================================================

extract_coefs <- function(fit, s) {
  b <- as.matrix(coef(fit, s = s))
  df <- data.frame(
    term = rownames(b),
    beta = as.numeric(b[,1]),
    stringsAsFactors = FALSE
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(beta)
    ) %>%
    arrange(desc(abs(beta)))
  df
}

coef_min <- extract_coefs(cvfit, lambda_min) %>% mutate(lambda = "min")
coef_1se <- extract_coefs(cvfit, lambda_1se) %>% mutate(lambda = "1se")

coef_all <- bind_rows(coef_min, coef_1se)

write_csv(coef_all, file.path(out_dir, paste0("LASSO_DILI_coef_OR_", run_tag, ".csv")))

# Save model object for reuse / risk scoring
saveRDS(cvfit, file.path(out_dir, paste0("LASSO_DILI_cvglmnet_", run_tag, ".rds")))

# Simple: non-zero terms summary
nz_min <- coef_min %>% filter(beta != 0)
nz_1se <- coef_1se %>% filter(beta != 0)

write_csv(nz_min, file.path(out_dir, paste0("LASSO_DILI_nonzero_terms_lambda_min_", run_tag, ".csv")))
write_csv(nz_1se, file.path(out_dir, paste0("LASSO_DILI_nonzero_terms_lambda_1se_", run_tag, ".csv")))

cat("\nNon-zero terms (lambda.min):", nrow(nz_min), "\n")
print(nz_min)

cat("\nNon-zero terms (lambda.1se):", nrow(nz_1se), "\n")
print(nz_1se)

# ============================================================
# 8) (Optional) Basic predictions + AUC (quick QC)
# ============================================================
# If you want AUROC/AUPRC later, we can add pROC/PRROC.
# Here we just export predicted probabilities for potential calibration plots.
pred_min <- as.numeric(predict(cvfit, newx = x, s = lambda_min, type = "response"))
pred_1se <- as.numeric(predict(cvfit, newx = x, s = lambda_1se, type = "response"))

pred_out <- data.frame(
  caseid = dat_model$caseid,
  DILI = y,
  pred_lambda_min = pred_min,
  pred_lambda_1se = pred_1se
)

write_csv(pred_out, file.path(out_dir, paste0("LASSO_DILI_predictions_", run_tag, ".csv")))

cat("\nDone. Outputs saved to:", out_dir, "\n")
