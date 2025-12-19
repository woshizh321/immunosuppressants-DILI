suppressPackageStartupMessages({
  library(glmnet)
  library(readr)
  library(dplyr)
})

source("02_model_P_LASSO/scripts/00_utils.R")

set.seed(202501)

# ---------- paths ----------
in_data <- "04_logistic/outputs/model_case_level_table.rds"
model_dir <- "02_model_P_LASSO/outputs/model"
out_dir   <- "02_model_P_LASSO/outputs/evaluation"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- load ----------
dat <- readRDS(in_data)
cvfit <- readRDS(file.path(model_dir, "ModelP_glmnet_fit.rds"))

y <- dat$DILI
x_vars <- setdiff(names(dat), c("DILI", "caseid"))
x <- sparse.model.matrix(~ . -1, data = dat[, x_vars])

# ---------- predictions ----------
p_min <- as.numeric(predict(cvfit, x, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cvfit, x, s = "lambda.1se", type = "response"))

# ---------- metrics ----------
m_min <- calc_metrics(y, p_min)
m_1se <- calc_metrics(y, p_1se)

metrics <- data.frame(
  model = c("lambda.min", "lambda.1se"),
  AUROC = c(m_min$AUROC, m_1se$AUROC),
  AUPRC = c(m_min$AUPRC, m_1se$AUPRC),
  Brier = c(m_min$Brier, m_1se$Brier)
)

write_csv(metrics, file.path(out_dir, "metrics_summary.csv"))

print(metrics)

# ---------- calibration ----------
cal_min <- make_calibration_table(y, p_min)
cal_1se <- make_calibration_table(y, p_1se)

write_csv(cal_min,
          file.path(out_dir, "calibration_table_lambda_min.csv"))
write_csv(cal_1se,
          file.path(out_dir, "calibration_table_lambda_1se.csv"))

log_msg("Step2 evaluation finished")
