## =========================================================
## Figure 3A — SOC-level signal summary (Dot plot) | FAERS
## Target SOC term: Hepatobiliary disorders
## x: log10(ROR), errorbar: log10(ROR_L)–log10(ROR_U)
## color: drug class (Morandi palette, locked)
## size: Chi2
## Export: PDF (vector) + PNG (300 dpi)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(stringr)
})

## -------------------- 1) Paths --------------------
indir  <- "D:/Projects/immunosuppressants_DILI/01_signal_detection/outputs/SOC"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure3"
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

target_term <- "Hepatobiliary disorders"

## -------------------- 3) Read all SOC files (QC-first) --------------------
files <- list.files(indir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files found in: ", indir)

read_one <- function(f) {
  x <- read_csv(f, show_col_types = FALSE)
  need <- c("a","b","c","d","ROR","ROR_L","ROR_U","Chi2","drug_class","level","term")
  miss <- setdiff(need, names(x))
  if (length(miss) > 0) stop("Missing columns in ", basename(f), ": ", paste(miss, collapse = ", "))
  x
}

dt <- map_dfr(files, read_one) %>%
  mutate(
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    level = as.character(level),
    term  = as.character(term),
    a = suppressWarnings(as.numeric(a)),
    ROR = suppressWarnings(as.numeric(ROR)),
    ROR_L = suppressWarnings(as.numeric(ROR_L)),
    ROR_U = suppressWarnings(as.numeric(ROR_U)),
    Chi2 = suppressWarnings(as.numeric(Chi2))
  ) %>%
  filter(level == "SOC", term == target_term)

if (nrow(dt) == 0) {
  stop("No rows found for SOC term: ", target_term,
       "\nCheck term spelling or level values in the SOC output files.")
}

## QC: one row per drug_class expected
dup <- dt %>% count(drug_class) %>% filter(n > 1)
if (nrow(dup) > 0) {
  warning("Multiple rows per drug_class detected. Keeping the one with max Chi2 per class.")
  dt <- dt %>% group_by(drug_class) %>% slice_max(Chi2, n = 1, with_ties = FALSE) %>% ungroup()
}

## QC: ensure required metrics valid
if (anyNA(dt$ROR) || any(dt$ROR <= 0)) stop("QC fail: ROR is NA or <= 0.")
if (anyNA(dt$ROR_L) || anyNA(dt$ROR_U) || any(dt$ROR_L <= 0) || any(dt$ROR_U <= 0)) {
  stop("QC fail: ROR_L/ROR_U invalid (NA or <= 0).")
}
if (any(dt$ROR_L > dt$ROR_U)) stop("QC fail: ROR_L > ROR_U found.")

## Prepare log scale
dt <- dt %>%
  mutate(
    log10ROR   = log10(ROR),
    log10ROR_L = log10(ROR_L),
    log10ROR_U = log10(ROR_U),
    drug_class = factor(drug_class, levels = rev(drug_levels)) # top-to-bottom nicer
  )

## -------------------- 4) Theme (consistent with Figure 2) --------------------
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

## -------------------- 5) Plot (Dot plot + CI) --------------------
p3a <- ggplot(dt, aes(y = drug_class, x = log10ROR, color = drug_class)) +
  ## Reference line: ROR = 1 (log10 = 0)
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  ## CI
  geom_errorbarh(aes(xmin = log10ROR_L, xmax = log10ROR_U), height = 0.18, linewidth = 0.9) +
  ## Point size reflects Chi2 (signal strength)
  geom_point(aes(size = Chi2), alpha = 0.95) +
  scale_color_manual(values = drug_colors, guide = "none") +
  scale_size_continuous(range = c(2.8, 7.5), name = expression(chi^2)) +
  labs(
    title = "SOC-level DILI signal (Hepatobiliary disorders) across immunosuppressant classes",
    x = expression(log[10](ROR)~~"(95% CI)"),
    y = NULL
  ) +
  theme_pub_box(base_size = 10) +
  theme(legend.position = "right")

print(p3a)

## -------------------- 6) Export --------------------
pdf_file <- file.path(outdir, "Figure3A_SOC_DotPlot_log10ROR_CI.pdf")
png_file <- file.path(outdir, "Figure3A_SOC_DotPlot_log10ROR_CI.png")

ggsave(pdf_file, p3a, width = 180, height = 110, units = "mm", device = cairo_pdf)
ggsave(png_file, p3a, width = 180, height = 110, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)

