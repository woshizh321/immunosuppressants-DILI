suppressPackageStartupMessages({
  library(pROC)
  library(PRROC)
  library(dplyr)
})

pred <- read.csv("02_model_F_LASSO/outputs/model/predictions.csv")

# QC
stopifnot(all(c("DILI","pred_lambda_1se") %in% colnames(pred)))
stopifnot(all(pred$DILI %in% c(0,1)))

# λ1se：link -> prob
pred$pred_prob <- plogis(pred$pred_lambda_1se)

# AUROC
roc_obj <- roc(pred$DILI, pred$pred_prob, quiet = TRUE)
auroc <- as.numeric(auc(roc_obj))

# AUPRC
pr <- pr.curve(
  scores.class0 = pred$pred_prob[pred$DILI == 1],
  scores.class1 = pred$pred_prob[pred$DILI == 0],
  curve = FALSE
)
auprc <- pr$auc.integral

# Brier (prob scale)
brier <- mean((pred$pred_prob - pred$DILI)^2)

model_full_metrics <- data.frame(
  model = "Model_Full (LASSO, lambda.1se)",
  AUROC = auroc,
  AUPRC = auprc,
  Brier = brier
)

print(model_full_metrics)

write.csv(model_full_metrics,
          "02_model_F_LASSO/outputs/model/Model_Full_metrics_lambda_1se.csv",
          row.names = FALSE)

