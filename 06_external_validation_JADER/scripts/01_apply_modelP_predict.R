# ============================================================
# Step4-01: Apply frozen FAERS Model-P (LASSO λ1se) to JADER
# FINAL FIX:
#  - training feature space from coef(fit, lambda.1se) (11 vars)
#  - robust add-missing-columns with matrix + dimnames
#  - strict checks before reordering/predict
#  - remove duplicate legacy blocks
# ============================================================

suppressPackageStartupMessages({
  library(glmnet)
  library(Matrix)
  library(data.table)
  library(here)
})

cat("Project root:", here(), "\n")

# ------------------------------------------------------------
# 0) Paths
# ------------------------------------------------------------
path_jader <- here("06_external_validation_JADER", "outputs", "data", "case_level_table_JADER.rds")
path_model <- here("03_model_P_LASSO", "outputs", "model", "modelP_glmnet_fit.rds")

stopifnot(file.exists(path_jader))
stopifnot(file.exists(path_model))

out_dir <- here("06_external_validation_JADER", "outputs", "prediction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "JADER_pred_prob_lambda_1se.rds")

# ------------------------------------------------------------
# 1) Load
# ------------------------------------------------------------
case_jader <- readRDS(path_jader)
setDT(case_jader)

fit <- readRDS(path_model)

cat("Loaded JADER cases:", nrow(case_jader), "\n")

# ------------------------------------------------------------
# 2) Minimal schema QC
# ------------------------------------------------------------
need_cols <- c(
  "caseid", "DILI", "sex_bin", "age_bin",
  "flag_calcineurin_inhibitors",
  "flag_antiproliferative_agents",
  "flag_mtor_inhibitors",
  "flag_corticosteroids",
  "flag_biologics"
)
miss <- setdiff(need_cols, names(case_jader))
if (length(miss) > 0) stop("Missing columns in JADER case-level table: ", paste(miss, collapse=", "))

# IMPORTANT: keep factor levels consistent with training convention used in your project
case_jader[, sex_bin := factor(sex_bin, levels = c("Female","Male","Other"))]
case_jader[, age_bin := factor(age_bin, levels = c("18-39","<18","40-64","65+"))]

# ------------------------------------------------------------
# 3) Build raw JADER design matrix (drop intercept)
# ------------------------------------------------------------
form_modelP <- ~ sex_bin + age_bin +
  flag_calcineurin_inhibitors +
  flag_antiproliferative_agents +
  flag_mtor_inhibitors +
  flag_corticosteroids +
  flag_biologics

X_jader <- model.matrix(form_modelP, data = case_jader)[, -1, drop = FALSE]
cat("Raw JADER design matrix:", dim(X_jader), "\n")
cat("Raw colnames(X_jader):\n")
print(colnames(X_jader))

# ------------------------------------------------------------
# 4) TRUE training feature names from coef(fit, lambda.1se)
# ------------------------------------------------------------
coef_mat <- as.matrix(coef(fit, s = "lambda.1se"))
coef_names <- rownames(coef_mat)
if (is.null(coef_names) || length(coef_names) == 0) stop("Cannot extract coef names from fit.")

train_cols <- setdiff(coef_names, "(Intercept)")
cat("TRUE training feature count (coef lambda.1se):", length(train_cols), "\n")
print(train_cols)

# ------------------------------------------------------------
# 5) Align X_jader to train_cols (robust)
# ------------------------------------------------------------

# 5.1 Add missing columns
missing_cols <- setdiff(train_cols, colnames(X_jader))
if (length(missing_cols) > 0) {
  cat("Adding missing columns as 0:\n", paste(missing_cols, collapse = ", "), "\n")
  for (v in missing_cols) {
    add_col <- matrix(0, nrow = nrow(X_jader), ncol = 1, dimnames = list(NULL, v))
    X_jader <- cbind(X_jader, add_col)
  }
}

# 5.2 Drop extra columns
extra_cols <- setdiff(colnames(X_jader), train_cols)
if (length(extra_cols) > 0) {
  cat("Dropping extra columns:\n", paste(extra_cols, collapse = ", "), "\n")
  X_jader <- X_jader[, setdiff(colnames(X_jader), extra_cols), drop = FALSE]
}

# 5.3 Hard check: all train_cols must exist now
still_missing <- setdiff(train_cols, colnames(X_jader))
if (length(still_missing) > 0) {
  cat("ERROR: After adding, still missing columns:\n", paste(still_missing, collapse=", "), "\n")
  cat("Current colnames(X_jader):\n")
  print(colnames(X_jader))
  stop("Alignment failed: some training columns are still missing.")
}

# 5.4 Reorder EXACTLY (now guaranteed safe)
X_jader <- X_jader[, train_cols, drop = FALSE]
cat("Aligned JADER design matrix:", dim(X_jader), "\n")

# ------------------------------------------------------------
# 6) Predict
# ------------------------------------------------------------
X_jader_sp <- Matrix::Matrix(X_jader, sparse = TRUE)
X_jader_sp <- as(X_jader_sp, "dgCMatrix")

pred_prob <- as.numeric(predict(fit, newx = X_jader_sp, s = "lambda.1se", type = "response"))
cat("Prediction summary:\n")
print(summary(pred_prob))

# ------------------------------------------------------------
# 7) Save
# ------------------------------------------------------------
pred_out <- case_jader[, .(caseid, DILI)]
pred_out[, pred_prob := pred_prob]

saveRDS(pred_out, out_file)
cat("Saved predictions to:\n", out_file, "\n")
cat("Step4-01 completed successfully.\n")

