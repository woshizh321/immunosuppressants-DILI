suppressPackageStartupMessages({
  library(Matrix)
  library(glmnet)
  library(readr)
  library(dplyr)
  library(doParallel)
})

source("02_model_P_LASSO/scripts/00_utils.R")

set.seed(202501)

# ---------- paths ----------
in_data <- "04_logistic/outputs/model_case_level_table.rds"
out_dir <- "02_model_P_LASSO/outputs/model"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- load data ----------
log_msg("Loading case-level data")
dat <- readRDS(in_data)

# ---------- QC ----------
stopifnot("DILI" %in% names(dat))
qc_binary(dat$DILI, "DILI")

log_msg("Total cases:", nrow(dat),
        "| DILI:", sum(dat$DILI),
        "| rate:", round(mean(dat$DILI), 4))

# ---------- define predictors ----------
y <- dat$DILI

x_vars <- setdiff(names(dat), c("DILI", "caseid"))
x <- sparse.model.matrix(~ . -1, data = dat[, x_vars])

log_msg("Design matrix:", nrow(x), "x", ncol(x))

# ---------- parallel ----------
ncore <- 6
cl <- makeCluster(ncore)
registerDoParallel(cl)

# ---------- LASSO ----------
log_msg("Running cv.glmnet")
cvfit <- cv.glmnet(
  x, y,
  family = "binomial",
  alpha = 1,
  nfolds = 10,
  parallel = TRUE,
  standardize = TRUE
)

stopCluster(cl)

saveRDS(cvfit, file.path(out_dir, "ModelP_glmnet_fit.rds"))

# ---------- extract coefficients ----------
extract_coef <- function(fit, lambda) {
  b <- as.matrix(coef(fit, s = lambda))
  dt <- data.frame(
    term = rownames(b),
    beta = as.numeric(b[,1]),
    stringsAsFactors = FALSE
  )
  dt <- dt %>% filter(beta != 0)
  dt$OR <- exp(dt$beta)
  dt
}

coef_min  <- extract_coef(cvfit, "lambda.min")
coef_1se  <- extract_coef(cvfit, "lambda.1se")

write_csv(coef_min,
          file.path(out_dir, "ModelP_lambda_min_coefficients.csv"))
write_csv(coef_1se,
          file.path(out_dir, "ModelP_lambda_1se_coefficients.csv"))

log_msg("Non-zero coefficients:",
        "lambda.min =", nrow(coef_min),
        "| lambda.1se =", nrow(coef_1se))

log_msg("Step2-ModelP fitting finished")
