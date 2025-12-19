suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(pROC)
  library(PRROC)
})

# ---------- logging ----------
log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ..., "\n")
}

# ---------- QC ----------
qc_binary <- function(x, name) {
  if (!all(x %in% c(0,1))) {
    stop("QC failed: ", name, " is not binary 0/1")
  }
}

# ---------- calibration ----------
make_calibration_table <- function(y, p, n_bins = 10) {
  
  dt <- data.table(y = y, p = p)
  
  # 计算分位点并去重
  qs <- unique(quantile(p, probs = seq(0, 1, length.out = n_bins + 1)))
  
  # 如果去重后箱数太少，直接按概率值分组
  if (length(qs) <= 2) {
    return(
      dt[, .(
        n = .N,
        obs = mean(y),
        pred = mean(p)
      ), by = p][order(pred)]
    )
  }
  
  dt[, bin := cut(p, breaks = qs, include.lowest = TRUE)]
  
  dt[, .(
    n = .N,
    obs = mean(y),
    pred = mean(p)
  ), by = bin][order(pred)]
}


# ---------- metrics ----------
calc_metrics <- function(y, p) {
  roc <- pROC::roc(y, p, quiet = TRUE)
  pr  <- PRROC::pr.curve(scores.class0 = p[y==1],
                         scores.class1 = p[y==0],
                         curve = FALSE)
  list(
    AUROC = as.numeric(roc$auc),
    AUPRC = pr$auc.integral,
    Brier = mean((p - y)^2)
  )
}

