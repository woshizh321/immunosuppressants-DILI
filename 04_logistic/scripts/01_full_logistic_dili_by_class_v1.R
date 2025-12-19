suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
})

# ============================================================
# 0) Inputs
# ============================================================
faers_path  <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target  <- "Hepatobiliary disorders"

source("99_shared/dict/immunosuppressants_drug_classes.R")

out_dir <- "04_logistic/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

duckdb_dir  <- "99_shared/tmp_duckdb"
dir.create(duckdb_dir, recursive = TRUE, showWarnings = FALSE)
duckdb_file <- file.path(duckdb_dir, "logistic_dili_work.duckdb")

run_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ============================================================
# 1) DuckDB connect (spill-safe) + QC schema
# ============================================================
con <- dbConnect(duckdb(), dbdir = duckdb_file)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")
dbExecute(con, sprintf("PRAGMA temp_directory='%s';",
                       normalizePath(duckdb_dir, winslash = "/", mustWork = FALSE)))
dbExecute(con, "PRAGMA enable_object_cache=true;")

schema <- dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s') LIMIT 1", faers_path))
need_cols <- c("caseid", "drugname", "role_cod", "soc_name", "sex_std", "age_group")
miss <- setdiff(need_cols, schema$column_name)
if (length(miss) > 0) stop("FAERS master missing required columns: ", paste(miss, collapse = ", "))
cat("QC passed: required columns exist.\n")

# ============================================================
# 2) Materialize slim tables (one-time)
# ============================================================
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE total_cases AS
  SELECT COUNT(DISTINCT caseid)::BIGINT AS n_total
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL;
", faers_path))
n_total <- dbGetQuery(con, "SELECT n_total FROM total_cases")$n_total[1]
cat("Total distinct cases:", n_total, "\n")

# case-level demographics
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_demo AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(sex_std) AS sex_std,
    MAX(age_group) AS age_group
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid;
", faers_path))

# case-level DILI outcome
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_dili AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(CASE WHEN soc_name = '%s' THEN 1 ELSE 0 END) AS DILI
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid;
", soc_target, faers_path))

# PS/SS immunosuppressant drug rows only
all_is_drugs <- unique(toupper(unlist(drug_classes)))
dbWriteTable(con, "all_is_drug_list",
             data.frame(drugname = all_is_drugs, stringsAsFactors = FALSE),
             overwrite = TRUE)

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE psss_is_rows AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname)) AS drugname
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND role_cod IN ('PS','SS')
    AND UPPER(TRIM(drugname)) IN (SELECT drugname FROM all_is_drug_list);
", faers_path))

# ============================================================
# 3) Build case-level exposure flags for 5 classes
# ============================================================
make_flag_tblname <- function(cls_name) {
  paste0("flag_", str_replace_all(tolower(cls_name), "[^a-z0-9]+", "_"))
}

flag_tables <- c()
for (cls in names(drug_classes)) {
  drug_vec <- unique(toupper(drug_classes[[cls]]))
  dbWriteTable(con, "drug_list_tmp",
               data.frame(drugname = drug_vec, stringsAsFactors = FALSE),
               overwrite = TRUE)
  
  flag_tbl <- make_flag_tblname(cls)
  dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE %s AS
    SELECT caseid, 1 AS %s
    FROM psss_is_rows
    WHERE drugname IN (SELECT drugname FROM drug_list_tmp)
    GROUP BY caseid;
  ", flag_tbl, flag_tbl))
  
  flag_tables <- c(flag_tables, flag_tbl)
}

cat("Exposure flag tables:\n")
print(flag_tables)

# ============================================================
# 4) Assemble modeling table (case-level)
# ============================================================
join_flags_sql <- paste(
  vapply(flag_tables, function(tn) sprintf("LEFT JOIN %s f_%s ON b.caseid = f_%s.caseid", tn, tn, tn),
         character(1)),
  collapse = "\n"
)

select_flags_sql <- paste(
  vapply(flag_tables, function(tn) sprintf("COALESCE(f_%s.%s, 0) AS %s", tn, tn, tn),
         character(1)),
  collapse = ",\n    "
)

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE model_case_table AS
  WITH base AS (
    SELECT caseid, DILI FROM case_dili
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

qc <- dbGetQuery(con, "
  SELECT COUNT(*) AS n_cases,
         SUM(DILI) AS n_dili,
         ROUND(1.0*SUM(DILI)/COUNT(*), 6) AS dili_rate
  FROM model_case_table
")
print(qc)

dat <- dbGetQuery(con, "SELECT * FROM model_case_table")
# ============================================================
# PATCH: Export case-level modeling table for downstream models
# ============================================================

# 只选择 Step2/Step3/JADER 需要的最小字段集合
case_level <- dbGetQuery(con, "
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    DILI,
    -- 与 Step1/Step2 一致的编码口径
    CASE
      WHEN sex_std IN ('M','Male') THEN 'Male'
      WHEN sex_std IN ('F','Female') THEN 'Female'
      ELSE 'Other'
    END AS sex_bin,
    CASE
      WHEN age_group IN ('<18','0-17','0-18','<18 years','<18y','<18years') THEN '<18'
      WHEN age_group IN ('18-39','18_39','18-39 years','18-39y','18-39years') THEN '18-39'
      WHEN age_group IN ('40-64','40_64','40-64 years','40-64y','40-64years') THEN '40-64'
      WHEN age_group IN ('65+','>=65','65plus','65-','65以上') THEN '65+'
      ELSE NULL
    END AS age_bin,
    flag_calcineurin_inhibitors,
    flag_antiproliferative_agents,
    flag_mtor_inhibitors,
    flag_corticosteroids,
    flag_biologics
  FROM model_case_table
")

# 丢弃无法映射年龄的 case（与你 Step1/Step2 口径一致）
case_level <- subset(case_level, !is.na(age_bin))

# 基本 QC
stopifnot(all(case_level$DILI %in% c(0,1)))
stopifnot(all(case_level$flag_calcineurin_inhibitors %in% c(0,1)))
stopifnot(all(case_level$flag_antiproliferative_agents %in% c(0,1)))
stopifnot(all(case_level$flag_mtor_inhibitors %in% c(0,1)))
stopifnot(all(case_level$flag_corticosteroids %in% c(0,1)))
stopifnot(all(case_level$flag_biologics %in% c(0,1)))

# 固定 factor 参考水平（与 Step2/Step3 一致）
case_level$sex_bin <- factor(case_level$sex_bin,
                             levels = c("Female","Male","Other"))
case_level$age_bin <- factor(case_level$age_bin,
                             levels = c("18-39","<18","40-64","65+"))

# 保存为统一输入
out_path <- "04_logistic/outputs/model_case_level_table.rds"
saveRDS(case_level, out_path)

cat("Saved case-level modeling table to:\n", out_path, "\n")
cat("Rows:", nrow(case_level),
    "| DILI:", sum(case_level$DILI),
    "| Rate:", round(mean(case_level$DILI), 4), "\n")

# ============================================================
# 4.5) SPEED-UP: aggregate to pattern counts in DuckDB
#      (sex/age mapping + group-by)
# ============================================================

# 注意：如果你MASTER的 sex_std/age_group 取值略不同，
# 可以在这里扩展 CASE WHEN 的映射集合，但不要在R里逐行映射。

flag_select <- paste(flag_tables, collapse = ", ")
flag_group  <- paste(flag_tables, collapse = ", ")

dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE model_pattern_counts AS
  WITH t AS (
    SELECT
      DILI,
      CASE
        WHEN sex_std IN ('M','Male') THEN 'Male'
        WHEN sex_std IN ('F','Female') THEN 'Female'
        ELSE 'Other'
      END AS sex_bin,
      CASE
        WHEN age_group IN ('<18','0-17','0-18','<18 years','<18y','<18years') THEN '<18'
        WHEN age_group IN ('18-39','18_39','18-39 years','18-39y','18-39years') THEN '18-39'
        WHEN age_group IN ('40-64','40_64','40-64 years','40-64y','40-64years') THEN '40-64'
        WHEN age_group IN ('65+','>=65','65plus','65-','65以上') THEN '65+'
        ELSE NULL
      END AS age_bin,
      %s
    FROM model_case_table
  )
  SELECT
    sex_bin,
    age_bin,
    %s,
    COUNT(*)::BIGINT AS n,
    SUM(DILI)::BIGINT AS n_dili
  FROM t
  WHERE age_bin IS NOT NULL
  GROUP BY sex_bin, age_bin, %s;
", flag_select, flag_select, flag_group))

pat <- dbGetQuery(con, "SELECT * FROM model_pattern_counts")

cat("Pattern rows:", nrow(pat), "\n")
print(head(pat, 10))


# ============================================================
# 5) QC-first encoding (pattern-level): sex, age, flags, counts
# ============================================================

# basic QC
stopifnot(all(c("sex_bin","age_bin","n","n_dili") %in% names(pat)))
if (any(pat$n < 1)) stop("Found pattern with n < 1 (should not happen).")
if (any(pat$n_dili < 0) || any(pat$n_dili > pat$n)) stop("Invalid n_dili counts found.")

N_used <- sum(pat$n)
N_dili <- sum(pat$n_dili)

cat("N_used (after age mapping):", N_used, "\n")
cat("N_DILI (after age mapping):", N_dili, "\n")
cat("DILI rate:", round(N_dili / N_used, 6), "\n")

# factor levels (set references)
pat$sex_bin <- factor(pat$sex_bin, levels = c("Female","Male","Other"))
pat$age_bin <- factor(pat$age_bin, levels = c("18-39","<18","40-64","65+"))

# ensure flags are integer 0/1
for (fc in flag_tables) {
  pat[[fc]] <- as.integer(pat[[fc]])
  if (any(!pat[[fc]] %in% c(0L, 1L))) {
    stop("Flag column not binary 0/1: ", fc)
  }
}

# record final design columns for traceability
design_cols <- c("n", "n_dili", "sex_bin", "age_bin", flag_tables)
write_csv(data.frame(column = design_cols),
          file.path(out_dir, paste0("FAERS_logistic_fullmodel_design_matrix_cols_", run_tag, ".csv")))


# ============================================================
# 6) Fit full logistic (Model-E) using aggregated binomial counts
# ============================================================

form_pat <- as.formula(paste(
  "cbind(n_dili, n - n_dili) ~ sex_bin + age_bin +",
  paste(flag_tables, collapse = " + ")
))

fit <- glm(form_pat, data = pat, family = binomial())

saveRDS(fit, file.path(out_dir, paste0("FAERS_logistic_fullmodel_model_", run_tag, ".rds")))


# ============================================================
# 7) Export OR/CI (Wald) + p-values
# ============================================================
sm <- summary(fit)$coefficients
res <- data.frame(
  term = rownames(sm),
  beta = sm[, "Estimate"],
  se   = sm[, "Std. Error"],
  z    = sm[, "z value"],
  p_value = sm[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# Wald 95% CI
res <- res %>%
  mutate(
    OR = exp(beta),
    CI_low = exp(beta - 1.96*se),
    CI_high = exp(beta + 1.96*se),
    N_used = N_used,
    N_DILI = N_dili
  ) %>%
  arrange(desc(abs(beta)))

out_csv <- file.path(out_dir, paste0("FAERS_logistic_fullmodel_OR_CI_", run_tag, ".csv"))
write_csv(res, out_csv)

cat("Saved:", out_csv, "\n")
print(head(res, 20))
cat("Done.\n")

