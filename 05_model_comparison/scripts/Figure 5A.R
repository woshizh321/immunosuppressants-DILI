suppressPackageStartupMessages({
  library(tidyverse)
})

# =========================
# 1) 读入 calibration table
# =========================
cal <- read.csv(
  "D:/Projects/immunosuppressants_DILI/06_external_validation_JADER/outputs/evaluation/external_calibration_table.csv",
  stringsAsFactors = FALSE
)

# QC
stopifnot(all(c("bin","n","obs","pred") %in% colnames(cal)))

# 清洗数值
cal <- cal %>%
  mutate(
    n = as.numeric(n),
    obs_num  = as.numeric(gsub("[^0-9eE\\+\\-\\.]", "", obs)),
    pred_num = as.numeric(gsub("[^0-9eE\\+\\-\\.]", "", pred))
  )

# 计算 binomial SE 与 95% CI
cal <- cal %>%
  mutate(
    se_obs  = sqrt(pmax(obs_num * (1 - obs_num), 0) / n),
    ci_low = pmax(obs_num - 1.96 * se_obs, 0),
    ci_high= pmin(obs_num + 1.96 * se_obs, 1)
  )

cal$bin <- factor(cal$bin, levels = cal$bin)

# =========================
# 2) 指标（你已给出的数值）
# =========================
AUROC  <- 0.516565188
AUPRC  <- 0.079819569
Brier  <- 0.072734064
CIL    <- -0.045705721

anno_txt <- sprintf(
  "AUROC = %.3f\nAUPRC = %.3f\nBrier = %.3f\nCal-in-the-large = %.3f",
  AUROC, AUPRC, Brier, CIL
)

# =========================
# 3) 作图（升级版 5A）
# =========================
p5a <- ggplot(cal, aes(x = pred_num, y = obs_num)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0,
    linewidth = 0.6,
    color = "#1F77B4",
    alpha = 0.9
  ) +
  geom_point(
    size = 2.8,
    color = "#1F77B4",
    alpha = 0.95
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = anno_txt,
    hjust = 1.05, vjust = -0.05,
    size = 3.2
  ) +
  labs(
    x = "Predicted DILI risk",
    y = "Observed DILI risk (95% CI)",
    title = "External validation and calibration in JADER"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(6, 10, 6, 6)  # 右下角留空间给注释
  )

print(p5a)

# =========================
# 4) 导出
# =========================
out_dir <- "D:/Projects/immunosuppressants_DILI/06_external_validation_JADER/outputs/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(out_dir, "Figure5A_JADER_calibration_summary.png"),
  p5a, width = 5.2, height = 4.8, dpi = 300
)

ggsave(
  file.path(out_dir, "Figure5A_JADER_calibration_summary.pdf"),
  p5a, width = 5.2, height = 4.8
)

