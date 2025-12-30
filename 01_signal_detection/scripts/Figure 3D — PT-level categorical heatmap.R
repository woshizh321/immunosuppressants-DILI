## =========================================================
## Figure 3D — PT-level categorical heatmap (FAERS)
## Fill: ROR category (ROR>1 / ROR<1 / NA)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/01_signal_detection/outputs/PT/FAERS_PT_under_topHLT_under_hepatobiliary_by_class_signal_table.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure3"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Drug order (LOCKED) --------------------
drug_levels <- c(
  "Calcineurin inhibitors",
  "mTOR inhibitors",
  "Antiproliferative agents",
  "Corticosteroids",
  "Biologics"
)

## -------------------- 3) Read + QC --------------------
dt <- read_csv(infile, show_col_types = FALSE)

required_cols <- c("pt_name","ROR","drug_class","level","soc")
miss <- setdiff(required_cols, names(dt))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt %>%
  mutate(
    pt_name    = as.character(pt_name),
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    level      = as.character(level),
    soc        = as.character(soc),
    ROR        = suppressWarnings(as.numeric(ROR))
  ) %>%
  filter(level == "PT_under_topHLT")

if (nrow(dt) == 0) stop("No PT rows after filtering level=='PT_under_topHLT'.")

## -------------------- 4) ROR categorization --------------------
dt <- dt %>%
  mutate(
    ROR_cat = case_when(
      is.na(ROR)        ~ "ROR = NA",
      ROR > 1           ~ "ROR > 1",
      ROR < 1           ~ "ROR < 1",
      TRUE              ~ "ROR = NA"
    )
  )

## -------------------- 5) Select PT terms to display --------------------
TOP_N <- 30L

pt_use <- dt %>%
  filter(!is.na(ROR) & ROR > 1) %>%
  group_by(pt_name) %>%
  summarise(max_ROR = max(ROR, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_ROR)) %>%          # 也更合理：优先展示强信号
  slice_head(n = TOP_N) %>%
  pull(pt_name)

## -------------------- 6) Build plot table using ORIGINAL pt_name as key --------------------
dt_plot <- dt %>%
  filter(pt_name %in% pt_use) %>%
  mutate(
    pt_name = factor(pt_name, levels = rev(pt_use))  # 控制y轴顺序
  ) %>%
  tidyr::complete(
    pt_name,
    drug_class = factor(drug_levels, levels = drug_levels),
    fill = list(ROR_cat = "ROR = NA")
  )

## （可选但强烈建议）检查：原本 ROR>1 的单元格是否仍然是 ROR>1
check_mis <- dt_plot %>%
  filter(!is.na(ROR) & ROR > 1, ROR_cat != "ROR > 1")
if (nrow(check_mis) > 0) {
  print(check_mis %>% select(pt_name, drug_class, ROR, ROR_cat) %>% head(20))
  stop("QC fail: some ROR>1 cells are not categorized as 'ROR > 1'.")
}

## -------------------- 7) Plot --------------------
p3d <- ggplot(dt_plot, aes(x = drug_class, y = pt_name, fill = ROR_cat)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = pt_colors,
    name = "ROR category",
    breaks = c("ROR > 1", "ROR < 1", "ROR = NA")
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 38)) +  # ✅ wrap只在显示层做
  labs(
    title = "PT-level categorical DILI signal map under hepatobiliary disorders (FAERS)",
    x = NULL, y = NULL
  ) +
  theme_pub_box(base_size = 10)

print(p3d)



## -------------------- 9) Export --------------------
ggsave(file.path(outdir, "Figure3D_PT_Categorical_Heatmap.pdf"),
       p3d, width = 190, height = 190, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure3D_PT_Categorical_Heatmap.png"),
       p3d, width = 190, height = 190, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

