## =========================================================
## Figure 3B — HLGT bubble heatmap (ROR color, a size, border=ROR_L>1)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(stringr)
})

infile <- "D:/Projects/immunosuppressants_DILI/01_signal_detection/outputs/HLGT/FAERS_HLGT_under_hepatobiliary_by_class_signal_table.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure3"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

drug_levels <- c(
  "Calcineurin inhibitors",
  "mTOR inhibitors",
  "Antiproliferative agents",
  "Corticosteroids",
  "Biologics"
)

heat_cols <- c(
  "#d3d4cc",  # low (ROR ~ 0)
  "#F6F1EA",  # mid (ROR = 1)
  "#8E3232"   # high (ROR ~ 10)
)

theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text.x  = element_text(size = base_size - 1, color = "black", angle = 20, hjust = 1),
      axis.text.y  = element_text(size = base_size - 1, color = "black"),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text  = element_text(size = base_size - 1),
      panel.grid   = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## ---- Read + QC ----
dt <- read_csv(infile, show_col_types = FALSE)

need <- c("hlgt_name","a","ROR","ROR_L","drug_class","level","soc","Chi2")
miss <- setdiff(need, names(dt))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt %>%
  mutate(
    hlgt_name  = as.character(hlgt_name),
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    level      = as.character(level),
    soc        = as.character(soc),
    a     = suppressWarnings(as.numeric(a)),
    ROR   = suppressWarnings(as.numeric(ROR)),
    ROR_L = suppressWarnings(as.numeric(ROR_L)),
    Chi2  = suppressWarnings(as.numeric(Chi2))
  ) %>%
  filter(level == "HLGT")

if (anyNA(dt$ROR) || any(dt$ROR < 0)) stop("QC fail: ROR NA or <0.")
if (anyNA(dt$a) || any(dt$a < 0)) stop("QC fail: a NA or <0.")
if (anyNA(dt$ROR_L) || any(dt$ROR_L < 0)) stop("QC fail: ROR_L NA or <0.")

## ---- Select top HLGT terms to display (by max Chi2) ----
TOP_N <- 20L
hlgt_use <- dt %>%
  group_by(hlgt_name) %>%
  summarise(max_Chi2 = max(Chi2, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_Chi2)) %>%
  slice_head(n = TOP_N) %>%
  pull(hlgt_name)

dt_plot <- dt %>%
  filter(hlgt_name %in% hlgt_use) %>%
  mutate(
    hlgt_name_wrapped = stringr::str_wrap(hlgt_name, width = 34),
    hlgt_name_wrapped = factor(hlgt_name_wrapped, levels = rev(unique(hlgt_name_wrapped))),
    stable = (ROR_L > 1),
    stroke = ifelse(stable, 0.5, 0)  # 描边：稳定信号
  ) %>%
  tidyr::complete(
    hlgt_name_wrapped = factor(levels(.$hlgt_name_wrapped), levels = levels(.$hlgt_name_wrapped)),
    drug_class = factor(drug_levels, levels = drug_levels),
    fill = list(a = 0, ROR = NA_real_, ROR_L = NA_real_, stable = FALSE, stroke = 0)
  )

## ---- Plot ----
p3b <- ggplot(dt_plot, aes(x = drug_class, y = hlgt_name_wrapped)) +
  geom_point(
    aes(size = a, fill = ROR, stroke = stroke),
    shape = 21, color = "black", alpha = 0.95, na.rm = TRUE
  ) +
  scale_fill_gradient2(
    low = heat_cols[1], mid = heat_cols[2], high = heat_cols[3],
    midpoint = 1,
    limits = c(0, 10),
    oob = scales::squish,
    breaks = c(0, 1, 2, 4, 8),
    name = "ROR",
    na.value = "grey95"
  ) +
  scale_size_continuous(
    range = c(1.2, 10),
    trans = "sqrt",  # a 跨度通常大，用 sqrt 更稳
    breaks = pretty_breaks(n = 4),
    name = "a (events)"
  ) +
  labs(
    title = "HLGT-level signal bubble map under Hepatobiliary disorders (FAERS)",
    x = NULL, y = NULL
  ) +
  theme_pub_box(base_size = 10)

print(p3b)

## ---- Export ----
ggsave(file.path(outdir, "Figure3B_HLGT_Bubble_ROR_SizeA_BorderStable.pdf"),
       p3b, width = 190, height = 150, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure3B_HLGT_Bubble_ROR_SizeA_BorderStable.png"),
       p3b, width = 190, height = 150, units = "mm", dpi = 300,
       device = ragg::agg_png, bg = "white")

