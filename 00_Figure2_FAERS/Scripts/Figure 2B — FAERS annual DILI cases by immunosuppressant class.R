## =========================================================
## Figure 2A — FAERS annual DILI cases by immunosuppressant class (2004–2024)
## Output: PDF (vector) + PNG (300 dpi)
## Style: Morandi-like muted palette (fixed & reusable)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/00_table1_FAERS/Table1_Annual_DILI_by_Class.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure2"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Global palette (Morandi, fixed) --------------------
## ⚠️ 后续所有涉及五类药物的图，直接复用这一段（保持一致）
drug_levels <- c(
  "Calcineurin inhibitors",
  "mTOR inhibitors",
  "Antiproliferative agents",
  "Corticosteroids",
  "Biologics"
)

## Morandi-like muted tones (hand-picked, colorblind-friendly-ish, low saturation)
drug_colors <- c(
  "Calcineurin inhibitors"      = "#6D8FA6",  # muted steel blue
  "mTOR inhibitors"             = "#B08B6E",  # muted warm brown
  "Antiproliferative agents"    = "#7C8A6A",  # muted olive green
  "Corticosteroids"             = "#A77A8B",  # muted dusty rose
  "Biologics"                   = "#8A7F9B"   # muted lavender-gray
)

## -------------------- 3) Read + QC (required) --------------------
dt <- readr::read_csv(infile, show_col_types = FALSE)

required_cols <- c("year","drug_class","dili_cases")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt <- dt %>%
  mutate(
    year = as.integer(year),
    drug_class = as.character(drug_class),
    dili_cases = suppressWarnings(as.numeric(dili_cases))
  )

if (anyNA(dt$year)) stop("QC fail: 'year' has NA after coercion.")
if (anyNA(dt$drug_class)) stop("QC fail: 'drug_class' has NA.")
if (anyNA(dt$dili_cases)) warning("QC warning: 'dili_cases' has NA (will drop in plotting).")

## Keep target years, enforce factor order
dt <- dt %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(drug_class = factor(drug_class, levels = drug_levels))

## QC: unexpected classes
unknown_classes <- setdiff(unique(as.character(dt$drug_class)), drug_levels)
unknown_classes <- unknown_classes[!is.na(unknown_classes)]
if (length(unknown_classes) > 0) {
  warning("QC warning: found drug_class not in drug_levels: ",
          paste(unknown_classes, collapse = ", "))
}

## Optional: make complete grid so lines don't break (keeps missing years as 0)
## If you prefer to leave gaps, comment out this block.
dt <- dt %>%
  tidyr::complete(
    year = 2004:2024,
    drug_class = factor(drug_levels, levels = drug_levels),
    fill = list(dili_cases = 0)
  )

## -------------------- 4) Theme (publication, reusable) --------------------
theme_pub <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size - 1, color = "black"),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text  = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "grey88"),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## -------------------- 5) Plot --------------------
p2a <- ggplot(dt, aes(x = year, y = dili_cases, color = drug_class, group = drug_class)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_color_manual(values = drug_colors, name = "Drug class") +
  scale_x_continuous(
    breaks = seq(2004, 2024, by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si("symbol")),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Annual FAERS DILI reports by immunosuppressant class (2004–2024)",
    x = "Year",
    y = "DILI cases (case-level)"
  ) +
  theme_pub(base_size = 10) +
  theme(legend.position = "right")

print(p2a)

## -------------------- 6) Export: PDF (vector) + PNG (300 dpi) --------------------
pdf_file <- file.path(outdir, "Figure2A_FAERS_Annual_DILI_by_Class.pdf")
png_file <- file.path(outdir, "Figure2A_FAERS_Annual_DILI_by_Class.png")

ggsave(pdf_file, p2a, width = 180, height = 120, units = "mm",
       device = cairo_pdf)  # vector PDF

ggsave(png_file, p2a, width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)

