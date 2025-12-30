## =========================================================
## Figure 2B — Age distribution by class (Median bar + IQR error bar)
## Style: same Morandi palette as Fig 2A
## Add: black panel border + white median label in bar
## Export: PDF (vector) + PNG (300 dpi)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/00_table1_FAERS/Table1_Age_by_Class.csv"
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

## Morandi-like muted palette (fixed across all 5 classes)
drug_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "mTOR inhibitors"          = "#B08B6E",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "Biologics"                = "#8A7F9B"
)

## -------------------- 3) Read + QC (QC-first) --------------------
dt <- read_csv(infile, show_col_types = FALSE)

required_cols <- c("drug_class","n_nonmissing","median","q1","q3")
miss <- setdiff(required_cols, names(dt))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt %>%
  mutate(
    drug_class = factor(drug_class, levels = drug_levels),
    median = as.numeric(median),
    q1 = as.numeric(q1),
    q3 = as.numeric(q3),
    n_nonmissing = suppressWarnings(as.integer(n_nonmissing))
  )

if (anyNA(dt$median) || anyNA(dt$q1) || anyNA(dt$q3)) {
  stop("QC fail: NA exists in median/q1/q3 after coercion.")
}

## QC: q1 <= median <= q3
bad_iqr <- dt %>% filter(!(q1 <= median & median <= q3))
if (nrow(bad_iqr) > 0) {
  stop("QC fail: found rows where q1 <= median <= q3 is violated.")
}

## -------------------- 4) Theme (same + black panel border) --------------------
theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size - 1, color = "black"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "grey88"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## -------------------- 5) Plot: Median bar + IQR error bar + white label --------------------
p2b <- ggplot(dt, aes(x = drug_class, y = median, fill = drug_class)) +
  ## Median bar
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  ## IQR error bar (q1 to q3)
  geom_errorbar(
    aes(ymin = q1, ymax = q3),
    width = 0.18,
    linewidth = 0.9,
    color = "black"
  ) +
  ## Median label inside bar
  geom_text(
    aes(label = paste0("Median = ", median)),
    color = "white",
    fontface = "bold",
    size = 3.2,
    vjust = 1.6
  ) +
  scale_fill_manual(values = drug_colors) +
  labs(
    title = "Age distribution of FAERS DILI cases by immunosuppressant class",
    x = NULL,
    y = "Age (years)"
  ) +
  theme_pub_box(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p2b)

## -------------------- 6) Export: PDF + PNG --------------------
pdf_file <- file.path(outdir, "Figure2B_Age_MedianBar_IQR.pdf")
png_file <- file.path(outdir, "Figure2B_Age_MedianBar_IQR.png")

ggsave(pdf_file, p2b,
       width = 180, height = 120, units = "mm",
       device = cairo_pdf)

ggsave(png_file, p2b,
       width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)

