suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

## -------------------- 1) 固定配色（与全文一致） --------------------
drug_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "mTOR inhibitors"          = "#B08B6E",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "Biologics"                = "#8A7F9B"
)

## -------------------- 2) 输入你给的 JADER 总量数据 --------------------
df <- tibble::tribble(
  ~drug_class,                 ~n,
  "Corticosteroids",            46301,
  "Antiproliferative agents",   40038,
  "Calcineurin inhibitors",     25492,
  "Biologics",                  10633,
  "mTOR inhibitors",             5898
)

N_total <- 80362

df <- df %>%
  mutate(
    pct = 100 * n / N_total
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    drug_class = factor(drug_class, levels = drug_class),
    label = sprintf("%s\n(%.1f%%)", comma(n), pct)
  )

## -------------------- 3) 主题：白底 + 黑框（与你前文一致） --------------------
theme_pub_box <- function(base_size = 11, base_family = "Times New Roman") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text  = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey30"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

## -------------------- 4) 绘图 --------------------
p <- ggplot(df, aes(x = drug_class, y = n, fill = drug_class)) +
  geom_col(width = 0.72, color = "black", linewidth = 0.35) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 3.3,
    family = "Times New Roman",
    fontface = "bold"
  ) +
  scale_fill_manual(values = drug_colors, guide = "none") +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si("symbol")),
    limits = c(0, max(df$n) * 1.18),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Class contribution of immunosuppressant-associated reports in JADER",
    subtitle = sprintf("Total N = %s", comma(N_total)),
    x = NULL,
    y = "Number of reports"
  ) +
  theme_pub_box(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

print(p)

## -------------------- 5) 导出（按你的工程目录，可改） --------------------
out_dir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure5_JADER"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "Figure5D_JADER_ClassContribution_N.png"),
       p, width = 170, height = 110, units = "mm", dpi = 300, bg = "white")
ggsave(
  filename = file.path(out_dir, "Figure5D_JADER_ClassContribution_N.pdf"),
  plot = p,
  width = 170,
  height = 110,
  units = "mm",
  device = cairo_pdf  # 关键点：指定使用 cairo 设备
)
