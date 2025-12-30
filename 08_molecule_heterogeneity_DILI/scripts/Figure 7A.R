suppressPackageStartupMessages({
  library(tidyverse)
})

# =========================
# 1) 读入数据
# =========================
dat <- read.csv(
  "D:/Projects/immunosuppressants_DILI/08_molecule_heterogeneity_DILI/outputs/results/molecule_heterogeneity_OR_CI.csv",
  stringsAsFactors = FALSE
)

# QC
stopifnot(all(c(
  "drug_class","model_type","reference_molecule",
  "term","OR","CI_low","CI_high"
) %in% colnames(dat)))

# =========================
# 2) 整理 molecule 名称
# =========================
dat <- dat %>%
  mutate(
    molecule = term %>%
      gsub("^molecule_", "", .) %>%
      gsub("Multiple", "Multiple exposure", .),
    model = case_when(
      model_type == "within_class_factor" ~ "Within-class",
      model_type == "global_dummy_secondary" ~ "Global",
      TRUE ~ model_type
    )
  )

# 只保留两种模型（用于 Figure 7A）
dat_plot <- dat %>%
  filter(model %in% c("Within-class", "Global"))

# =========================
# 3) 分面内排序（按 within-class OR）
# =========================
order_tbl <- dat_plot %>%
  filter(model == "Within-class") %>%
  arrange(desc(OR)) %>%
  select(drug_class, molecule) %>%
  group_by(drug_class) %>%
  mutate(order_id = row_number()) %>%
  ungroup()

dat_plot <- dat_plot %>%
  left_join(order_tbl, by = c("drug_class","molecule")) %>%
  group_by(drug_class) %>%
  mutate(
    molecule_f = factor(
      molecule,
      levels = unique(molecule[order(order_id)])
    )
  ) %>%
  ungroup()

# =========================
# 4) 颜色与形状（统一风格）
# =========================
cols <- c(
  "Within-class" = "#1F77B4",
  "Global"       = "#D62728"
)

shapes <- c(
  "Within-class" = 16,
  "Global"       = 17
)

# =========================
# 5) 绘图
# =========================
p7a <- ggplot(
  dat_plot,
  aes(
    x = OR,
    y = molecule_f,
    color = model,
    shape = model
  )
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.2,
    linewidth = 0.7,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.6,
    position = position_dodge(width = 0.5)
  ) +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 4),
    limits = c(0.4, 4.5)
  ) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  facet_wrap(
    ~ drug_class,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Molecule-level heterogeneity of DILI risk across immunosuppressant classes",
    x = "Odds ratio (log scale)",
    y = NULL,
    color = "Model",
    shape = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

print(p7a)

# =========================
# 6) 导出
# =========================
out_dir <- "D:/Projects/immunosuppressants_DILI/08_molecule_heterogeneity_DILI/outputs/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(out_dir, "Figure7A_molecule_heterogeneity_forest.png"),
  p7a, width = 10.5, height = 7.2, dpi = 300
)
ggsave(
  file.path(out_dir, "Figure7A_molecule_heterogeneity_forest.pdf"),
  p7a, width = 10.5, height = 7.2
)

