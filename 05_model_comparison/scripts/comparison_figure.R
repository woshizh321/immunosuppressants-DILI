suppressPackageStartupMessages({
  library(tidyverse)
})

# =========================
# 1) 读入数据
# =========================
df <- read.csv(
  "D:/Projects/immunosuppressants_DILI/05_model_comparison/outputs/OR_comparison_clean.csv",
  stringsAsFactors = FALSE
)

# QC
stopifnot(all(c("term", "rank_lasso", "rank_logistic") %in% colnames(df)))

# =========================
# 2) 标注变量类型（用于配色：拆分到具体药物类别）
# =========================
df <- df %>%
  mutate(
    var_type = case_when(
      term == "flag_calcineurin_inhibitors"   ~ "Calcineurin inhibitors",
      term == "flag_antiproliferative_agents" ~ "Antiproliferative agents",
      term == "flag_corticosteroids"          ~ "Corticosteroids",
      term == "flag_mtor_inhibitors"          ~ "mTOR inhibitors",
      term == "flag_biologics"                ~ "Biologics",
      grepl("^sex_", term)                    ~ "Sex",
      grepl("^age_", term)                    ~ "Age",
      TRUE                                    ~ "Other"
    )
  )

df$var_type <- factor(
  df$var_type,
  levels = c(
    "Calcineurin inhibitors",
    "Antiproliferative agents",
    "Corticosteroids",
    "mTOR inhibitors",
    "Biologics",
    "Sex",
    "Age",
    "Other"
  )
)


# =========================
# 3) 颜色方案（与 OR 森林图一致，按药物类别）
# =========================
cols <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "mTOR inhibitors"          = "#B08B6E",
  "Biologics"                = "#8A7F9B",
  "Sex"                      = "grey40",
  "Age"                      = "grey60",
  "Other"                    = "grey80"
)


# =========================
# 4) 绘图
# =========================
p4b <- ggplot(
  df,
  aes(
    x = rank_lasso,
    y = rank_logistic,
    color = var_type
  )
) +
  geom_point(size = 3.2, alpha = 0.9) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +
  geom_text(
    data = subset(df, var_type == "Drug exposure"),
    aes(label = gsub("^flag_", "", term)),
    vjust = -0.7,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = cols) +
  scale_x_reverse(
    breaks = seq(1, max(df$rank_lasso, na.rm = TRUE)),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_y_reverse(
    breaks = seq(1, max(df$rank_logistic, na.rm = TRUE)),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    x = "LASSO importance rank (λ\u2081se)",
    y = "Logistic regression OR rank",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(6, 6, 6, 6)
  )

print(p4b)

# =========================
# 5) 导出（投稿级）
# =========================
out_dir <- "05_model_comparison/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(out_dir, "Figure4B_LASSO_vs_Logistic_rank_consistency.png"),
  plot = p4b,
  width = 4.6,
  height = 4.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "Figure4B_LASSO_vs_Logistic_rank_consistency.pdf"),
  plot = p4b,
  width = 4.6,
  height = 4.4
)

