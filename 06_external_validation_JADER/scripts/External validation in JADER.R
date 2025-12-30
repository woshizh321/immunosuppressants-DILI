## ============================================================
## Step4-02: External validation in JADER
##  - Discrimination: AUROC / AUPRC
##  - Overall accuracy: Brier
##  - Calibration: quantile-based table + plot
##  - Optional: Decision Curve Analysis (DCA)
##
## Assumes objects available in session:
##  - pred_prob : numeric vector of predicted probabilities (JADER)
##  - case_jader: data.table with binary outcome DILI (0/1)
##
## Outputs saved to:
##  06_external_validation_JADER/outputs/evaluation/
## ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(pROC)
  library(PRROC)
  library(ggplot2)
  library(here)
})

## ---------------------------
## 0) Paths & basic checks
## ---------------------------
out_dir <- here("06_external_validation_JADER", "outputs", "evaluation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(exists("pred_prob"))
stopifnot(exists("case_jader"))
stopifnot(length(pred_prob) == nrow(case_jader))
stopifnot(all(!is.na(pred_prob)))

if (!("DILI" %in% names(case_jader))) {
  stop("case_jader must contain binary outcome column named 'DILI'")
}

y <- as.integer(case_jader$DILI)
p <- as.numeric(pred_prob)

cat(sprintf("External dataset size: %d | DILI events: %d | rate: %.4f\n",
            length(y), sum(y), mean(y)))

## ---------------------------
## 1) Discrimination metrics
## ---------------------------

## AUROC
roc_obj <- pROC::roc(response = y, predictor = p, quiet = TRUE, direction = "<")
auroc <- as.numeric(pROC::auc(roc_obj))

## AUPRC
## PRROC expects scores for positives and negatives separately
scores_pos <- p[y == 1]
scores_neg <- p[y == 0]
pr_obj <- PRROC::pr.curve(scores.class0 = scores_pos,
                          scores.class1 = scores_neg,
                          curve = FALSE)
auprc <- pr_obj$auc.integral

## Brier score
brier <- mean((p - y)^2)

metrics <- data.table(
  Dataset = "JADER",
  AUROC   = auroc,
  AUPRC   = auprc,
  Brier   = brier
)

fwrite(metrics, file = file.path(out_dir, "external_metrics_summary.csv"))
print(metrics)

## ---------------------------
## 2) Calibration (robust bins)
## ---------------------------

## Helper: safe quantile bins (avoids duplicate breaks)
make_bins_safe <- function(p, n_bins = 10) {
  qs <- quantile(p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  qs <- unique(qs)
  if (length(qs) <= 2) {
    ## fallback to equal-width bins if probabilities are very concentrated
    qs <- unique(seq(min(p), max(p), length.out = n_bins + 1))
  }
  cut(p, breaks = qs, include.lowest = TRUE)
}

n_bins <- 10
bin <- make_bins_safe(p, n_bins = n_bins)

cal_tab <- data.table(
  p = p,
  y = y,
  bin = bin
)[
  , .(
    n   = .N,
    obs = mean(y),
    pred = mean(p)
  ),
  by = bin
][order(pred)]

fwrite(cal_tab, file = file.path(out_dir, "external_calibration_table.csv"))

## Calibration-in-the-large
cal_in_large <- data.table(
  metric = "calibration_in_the_large",
  value  = mean(p) - mean(y)
)
fwrite(cal_in_large, file = file.path(out_dir, "external_calibration_in_the_large.csv"))

## ---------------------------
## 3) Calibration plot
## ---------------------------
p_cal <- ggplot(cal_tab, aes(x = pred, y = obs)) +
  geom_point(size = 2) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  coord_equal(xlim = range(c(cal_tab$pred, cal_tab$obs)),
              ylim = range(c(cal_tab$pred, cal_tab$obs))) +
  labs(
    title = "External calibration (JADER)",
    x = "Mean predicted risk",
    y = "Observed DILI rate"
  ) +
  theme_bw(base_size = 12)

ggsave(
  filename = file.path(out_dir, "external_calibration_plot.png"),
  plot = p_cal, width = 6, height = 6, dpi = 300
)

## ---------------------------
## 4) Optional: Decision Curve Analysis (DCA)
## ---------------------------
## Simple base-R implementation (no refitting)

do_dca <- TRUE
if (do_dca) {
  thresholds <- seq(0.01, 0.20, by = 0.005)
  
  dca_tab <- data.table(threshold = thresholds)[
    , `:=`(
      NB_model = sapply(thresholds, function(t) {
        tp <- sum(p >= t & y == 1)
        fp <- sum(p >= t & y == 0)
        n  <- length(y)
        tp/n - fp/n * (t / (1 - t))
      }),
      NB_all = sapply(thresholds, function(t) {
        tp <- sum(y == 1)
        fp <- sum(y == 0)
        n  <- length(y)
        tp/n - fp/n * (t / (1 - t))
      }),
      NB_none = 0
    )
  ]
  
  fwrite(dca_tab, file = file.path(out_dir, "external_DCA_table.csv"))
  
  p_dca <- ggplot(dca_tab, aes(x = threshold)) +
    geom_line(aes(y = NB_model, color = "Model")) +
    geom_line(aes(y = NB_all,   color = "Treat all")) +
    geom_line(aes(y = NB_none,  color = "Treat none")) +
    labs(
      title = "Decision Curve Analysis (JADER)",
      x = "Risk threshold",
      y = "Net benefit",
      color = ""
    ) +
    theme_bw(base_size = 12)
  
  ggsave(
    filename = file.path(out_dir, "external_DCA_plot.png"),
    plot = p_dca, width = 7, height = 5, dpi = 300
  )
}

cat("Step4-02 external validation finished.\n")

