suppressPackageStartupMessages({
  library(tidyverse)
})

## -------------------- 1) 固定配色（与全文一致） --------------------
drug_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "mTOR inhibitors"          = "#B08B6E",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "Biologics"                = "#8A7F9B"
)

## -------------------- 2) 手动录入本句数据 --------------------
df <- tibble::tribble(
  ~drug_class,                 ~n,    ~pct,
  "Calcineurin inhibitors",     7304,  24.9,
  "Corticosteroids",            6869,  23.5,
  "Biologics",                  6477,  22.1,
  "Antiproliferative agents",   5091,  17.4,
  "mTOR inhibitors",            3536,  12.1
)

N_total <- 29277

## 排序（从高到低）
df <- df %>%
  arrange(desc(pct)) %>%
  mutate(
    drug_class = factor(drug_class, levels = drug_class),
    label = sprintf("%s\n(%.1f%%)", scales::comma(n), pct)
  )

## -------------------- 3) 主题（白底 + 黑框，风格与前文一致） --------------------
theme_pub_box <- function(base_size = 11, base_family = "Times New Roman") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey30"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

## -------------------- 4) 绘图 --------------------
p <- ggplot(df, aes(x = drug_class, y = pct, fill = drug_class)) +
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
    limits = c(0, max(df$pct) * 1.18),
    breaks = seq(0, 30, 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Class contribution of immunosuppressant-associated reports",
    subtitle = sprintf("Total N = %s", scales::comma(N_total)),
    x = NULL,
    y = "Percentage of reports (%)"
  ) +
  theme_pub_box(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p)

## -------------------- 5) 导出 --------------------
out_dir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "Figure2A_FAERS_ClassContribution_bar.png"),
       p, width = 170, height = 110, units = "mm", dpi = 300, bg = "white")
# 1. 在绘图对象中去掉背景
p_clean <- p + theme(
  panel.background = element_blank(),     # 面板背景透明
  plot.background = element_blank(),      # 整张图背景透明
  legend.background = element_blank(),    # 图例背景透明
  rect = element_blank()                  # 所有矩形框透明
)

# 2. 重新导出 EPS
postscript(
  file = file.path(out_dir, "Figure2A_FAERS_Clean.eps"),
  width = 170/25.4, 
  height = 110/25.4,
  horizontal = FALSE, 
  onefile = FALSE, 
  paper = "special",
  family = "Helvetica",
  bg = "transparent" # 关键：设置设备背景为透明
)
print(p_clean)
dev.off()
