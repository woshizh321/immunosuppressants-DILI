## =========================================================
## Figure 2C — Sex distribution by class (FAERS)
## Fix: recompute pct from n/total; no dropped rows
## Update: colored fills for Female/Male/Other (Morandi-like)
## Export: PDF + PNG
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/00_table1_FAERS/Table1_Sex_by_Class.csv"
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

drug_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "mTOR inhibitors"          = "#B08B6E",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "Biologics"                = "#8A7F9B"
)

sex_levels <- c("Female", "Male", "Other")

## Morandi-like colored fills for sex (muted, low saturation)
sex_fills <- c(
  "Female" = "#C9A3A6",  # dusty rose
  "Male"   = "#8AA6B5",  # muted blue
  "Other"  = "#8E9B7A"   # muted olive
)

## -------------------- 3) Read + QC (QC-first) --------------------
dt <- read_csv(infile, show_col_types = FALSE)

required_cols <- c("drug_class","sex3","n","total")
miss <- setdiff(required_cols, names(dt))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt %>%
  mutate(
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    sex3 = factor(as.character(sex3), levels = sex_levels),
    n = suppressWarnings(as.numeric(n)),
    total = suppressWarnings(as.numeric(total))
  )

if (anyNA(dt$drug_class) || anyNA(dt$sex3)) stop("QC fail: NA in drug_class/sex3 after coercion.")
if (anyNA(dt$n) || anyNA(dt$total)) stop("QC fail: NA in n/total after coercion.")
if (any(dt$total <= 0)) stop("QC fail: total <= 0 exists.")

## complete missing combinations to keep all bars present
dt <- dt %>%
  tidyr::complete(
    drug_class = factor(drug_levels, levels = drug_levels),
    sex3 = factor(sex_levels, levels = sex_levels),
    fill = list(n = 0)
  ) %>%
  group_by(drug_class) %>%
  mutate(total = max(total, na.rm = TRUE)) %>%   # keep class total
  ungroup()

## Recompute pct robustly (avoid relying on provided pct column)
dt <- dt %>%
  mutate(
    pct = ifelse(total > 0, 100 * n / total, NA_real_)
  )

if (anyNA(dt$pct)) stop("QC fail: pct NA after recomputation.")

## QC: pct sum ~100 per class (rounding tolerance)
qc_sum <- dt %>%
  group_by(drug_class) %>%
  summarise(pct_sum = sum(pct), .groups = "drop")

bad <- qc_sum %>% filter(abs(pct_sum - 100) > 0.5)
if (nrow(bad) > 0) {
  warning("QC warning: pct not summing to 100 within some classes:\n",
          paste0(bad$drug_class, "=", round(bad$pct_sum, 2), collapse = "; "))
}

## Labels (avoid clutter)
dt <- dt %>%
  mutate(label = ifelse(pct >= 4, paste0(round(pct, 1), "%"), ""))

## -------------------- 4) Theme (same + black panel border) --------------------
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

## -------------------- 5) Plot --------------------
p2c <- ggplot(dt, aes(x = drug_class, y = pct, fill = sex3)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.0
  ) +
  scale_fill_manual(values = sex_fills, name = "Sex") +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),               # safe now because pct is recomputed
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Sex distribution of FAERS DILI cases by immunosuppressant class",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pub_box(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p2c)

## -------------------- 6) Export --------------------
pdf_file <- file.path(outdir, "Figure2C_Sex_Distribution_StackedPct.pdf")
png_file <- file.path(outdir, "Figure2C_Sex_Distribution_StackedPct.png")

ggsave(pdf_file, p2c,
       width = 180, height = 120, units = "mm",
       device = cairo_pdf)

ggsave(png_file, p2c,
       width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)

