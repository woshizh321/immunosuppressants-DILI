suppressPackageStartupMessages({
  library(tidyverse)
})

# =========================
# 1) 读入数据（用你给的路径）
# =========================
df <- read.csv(
  "D:/Projects/immunosuppressants_DILI/07_risk_portrait/outputs/tables/regimen_risk_portrait_table.csv",
  stringsAsFactors = FALSE
)

# QC
stopifnot(all(c("sex","age","regimen","OR_combo","risk_cat") %in% colnames(df)))

# =========================
# 2) regimen 缩写（论文友好）
# =========================
abbr_regimen <- function(x) {
  x %>%
    gsub("antiproliferative_agents", "APA", .) %>%
    gsub("calcineurin_inhibitors", "CNI", .) %>%
    gsub("corticosteroids", "CS", .) %>%
    gsub("mtor_inhibitors", "mTOR", .) %>%
    gsub("biologics", "BIO", .)
}

df <- df %>%
  mutate(regimen_abbr = abbr_regimen(regimen))

# =========================
# 3) 只取 Very high，并控制 regimen 数量（避免 facet 太多）
#    你可以改 top_n_regimens
# =========================
vh <- df %>%
  filter(tolower(risk_cat) == "very high")

# 每个 regimen 的出现频次 + 最大OR（用于排序/筛选）
reg_summ <- vh %>%
  group_by(regimen_abbr) %>%
  summarise(
    n_cells = n(),                # 有多少个 sex×age 单元落入 Very high
    max_or  = max(OR_combo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_cells), desc(max_or))

top_n_regimens <- 9  # ✅ 建议 6~12；太多就会很拥挤
keep_regs <- head(reg_summ$regimen_abbr, top_n_regimens)

vh_plot <- vh %>%
  filter(regimen_abbr %in% keep_regs) %>%
  mutate(
    sex = factor(sex, levels = c("Male","Female","Other")),
    age = factor(age, levels = c("<18","18-39","40-64","65+"))
  ) %>%
  left_join(reg_summ, by = "regimen_abbr") %>%
  mutate(
    regimen_abbr = factor(regimen_abbr, levels = keep_regs)  # facet顺序固定
  )

# =========================
# 4) 画“豆腐块矩阵”：每个 regimen 一个 3×4 小热图
# =========================
p6a <- ggplot(vh_plot, aes(x = age, y = sex, fill = OR_combo)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f", OR_combo)),
    size = 2.8,
    color = "black"
  ) +
  facet_wrap(~ regimen_abbr, ncol = 3) +
  scale_fill_gradient(
    low = "grey90",
    high = "#5E3C99",
    name = "OR"
  ) +
  labs(
    title = "Very-high risk portraits stratified by regimen (FAERS)",
    x = "Age group",
    y = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(p6a)

# =========================
# 5) 导出
# =========================
out_dir <- "D:/Projects/immunosuppressants_DILI/07_risk_portrait/outputs/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(out_dir, "Figure6A_VeryHigh_portraits_by_regimen.png"),
  p6a, width = 10.5, height = 7.2, dpi = 300
)
ggsave(
  file.path(out_dir, "Figure6A_VeryHigh_portraits_by_regimen.pdf"),
  p6a, width = 10.5, height = 7.2
)

