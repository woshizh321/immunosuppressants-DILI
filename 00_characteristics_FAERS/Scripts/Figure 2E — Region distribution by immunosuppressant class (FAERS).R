## =========================================================
## Figure 2D — Region distribution by immunosuppressant class (FAERS)
## Plot: 100% stacked bar (pct) with colored regions + white % labels
## Style: keep Fig 2A/2B/2C theme (Morandi + black panel border)
## Export: PDF (vector) + PNG (300 dpi)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/00_table1_FAERS/Table1_Region_by_Class.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure2"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Drug order & colors (LOCKED) --------------------
drug_levels <- c(
  "Calcineurin inhibitors",
  "mTOR inhibitors",
  "Antiproliferative agents",
  "Corticosteroids",
  "Biologics"
)

## keep for global consistency (used elsewhere), not directly used as fill in this plot
drug_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "mTOR inhibitors"          = "#B08B6E",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "Biologics"                = "#8A7F9B"
)

## -------------------- 3) Read + QC (QC-first) --------------------
dt <- read_csv(infile, show_col_types = FALSE)

required_cols <- c("drug_class","region","n","total")
miss <- setdiff(required_cols, names(dt))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt %>%
  mutate(
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    region = as.character(region),
    n = suppressWarnings(as.numeric(n)),
    total = suppressWarnings(as.numeric(total))
  )

if (anyNA(dt$drug_class)) stop("QC fail: drug_class has NA after coercion.")
if (anyNA(dt$region) || any(dt$region == "")) stop("QC fail: region has NA/empty.")
if (anyNA(dt$n) || anyNA(dt$total)) stop("QC fail: n/total has NA.")
if (any(dt$total <= 0)) stop("QC fail: total <= 0 exists.")

## complete missing combinations
region_levels <- sort(unique(dt$region))
## Prefer a sensible order if common labels exist
pref_order <- c("US","Europe","China","Japan","Other")
region_levels <- c(pref_order[pref_order %in% region_levels],
                   setdiff(region_levels, pref_order))
dt <- dt %>%
  tidyr::complete(
    drug_class = factor(drug_levels, levels = drug_levels),
    region = region_levels,
    fill = list(n = 0)
  ) %>%
  group_by(drug_class) %>%
  mutate(total = max(total, na.rm = TRUE)) %>%
  ungroup()

## recompute pct robustly
dt <- dt %>%
  mutate(pct = ifelse(total > 0, 100 * n / total, NA_real_))

if (anyNA(dt$pct)) stop("QC fail: pct NA after recomputation.")

## QC: pct sum ~100 per class
qc_sum <- dt %>%
  group_by(drug_class) %>%
  summarise(pct_sum = sum(pct), .groups = "drop")
bad <- qc_sum %>% filter(abs(pct_sum - 100) > 0.5)
if (nrow(bad) > 0) {
  warning("QC warning: pct not summing to 100 within some classes:\n",
          paste0(bad$drug_class, "=", round(bad$pct_sum, 2), collapse = "; "))
}

## labels (avoid clutter)
dt <- dt %>%
  mutate(label = ifelse(pct >= 4, paste0(round(pct, 1), "%"), ""))

## -------------------- 4) Region colors (muted, consistent style) --------------------
## If your table has only a few regions (US/Europe/China/Japan/Other), this palette works well.
## If there are more regions, extra ones will be auto-assigned from hues; better to extend manually.
region_colors <- c(
  "US"     = "#8AA6B5",  # muted blue
  "Europe" = "#C9A3A6",  # dusty rose
  "China"  = "#B7B08A",  # muted khaki
  "Japan"  = "#9AA7A0",  # muted sage/grey
  "Other"  = "#8F8F8F"   # neutral grey
)

## Fill missing colors if unexpected regions exist
missing_region_colors <- setdiff(region_levels, names(region_colors))
if (length(missing_region_colors) > 0) {
  auto_cols <- setNames(scales::hue_pal(l = 65, c = 45)(length(missing_region_colors)),
                        missing_region_colors)
  region_colors <- c(region_colors, auto_cols)
  warning("QC warning: unexpected regions found; auto colors assigned: ",
          paste(missing_region_colors, collapse = ", "))
}

dt <- dt %>% mutate(region = factor(region, levels = region_levels))

## -------------------- 5) Theme (same + black panel border) --------------------
theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size - 1, color = "black"),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text  = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "grey88"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## -------------------- 6) Plot: 100% stacked bar --------------------
p2d <- ggplot(dt, aes(x = drug_class, y = pct, fill = region)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.0
  ) +
  scale_fill_manual(values = region_colors, name = "Region") +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Regional distribution of FAERS DILI cases by immunosuppressant class",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pub_box(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "right"
  )

print(p2d)

## -------------------- 7) Export --------------------
pdf_file <- file.path(outdir, "Figure2D_Region_Distribution_StackedPct.pdf")
png_file <- file.path(outdir, "Figure2D_Region_Distribution_StackedPct.png")

ggsave(pdf_file, p2d,
       width = 180, height = 120, units = "mm",
       device = cairo_pdf)

ggsave(png_file, p2d,
       width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)
