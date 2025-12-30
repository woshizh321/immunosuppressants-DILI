## =========================================================
## Figure 5 (JADER) — Annual / Sex / Age by immunosuppressant class
## Style: MATCH Figure 2A/2B/2C (same theme, font, border, export mm)
## Export: PDF (vector) + PNG (300 dpi)
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
})

## -------------------- 1) Paths --------------------
in_dir <- "D:/Projects/immunosuppressants_DILI/00_table2_JADER"
annual_file <- file.path(in_dir, "JADER_Annual_cases_by_Class.csv")
sex_file    <- file.path(in_dir, "JADER_Sex_by_Class.csv")
age_file    <- file.path(in_dir, "JADER_Age_by_Class.csv")

outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure5_JADER"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Drug order & colors (LOCKED, same as Figure 2) --------------------
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

## Sex fills (same logic as Fig 2C, muted)
sex_levels <- c("Female", "Male", "Unknown")
sex_fills <- c(
  "Female"  = "#C9A3A6",  # dusty rose
  "Male"    = "#8AA6B5",  # muted blue
  "Unknown" = "#8E9B7A"   # muted olive
)

## -------------------- 3) Theme (copy from Figure 2 style) --------------------
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

theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_pub(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

## =========================================================
## Figure 5A — Annual reports by class (JADER)
## =========================================================
dtA <- read_csv(annual_file, show_col_types = FALSE)

## QC-first: accept either n_cases or cases column name, but require year + class + count
reqA1 <- c("year","drug_class","n_cases")
reqA2 <- c("year","drug_class","cases")
if (all(reqA1 %in% names(dtA))) {
  dtA <- dtA %>% rename(cases = n_cases)
} else if (all(reqA2 %in% names(dtA))) {
  # already cases
} else {
  stop("Figure5A QC fail: annual file must contain columns: ",
       paste(reqA1, collapse=", "), " OR ", paste(reqA2, collapse=", "))
}

dtA <- dtA %>%
  mutate(
    year = as.integer(year),
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    cases = suppressWarnings(as.numeric(cases))
  )

if (anyNA(dtA$year)) stop("Figure5A QC fail: year NA after coercion.")
if (anyNA(dtA$drug_class)) stop("Figure5A QC fail: drug_class NA after coercion.")
if (anyNA(dtA$cases)) warning("Figure5A QC warning: cases NA exists (will plot as gaps).")

## complete grid to avoid broken lines (same idea as Fig2A)
dtA <- dtA %>%
  tidyr::complete(
    year = min(dtA$year, na.rm = TRUE):max(dtA$year, na.rm = TRUE),
    drug_class = factor(drug_levels, levels = drug_levels),
    fill = list(cases = 0)
  )

p5a <- ggplot(dtA, aes(x = year, y = cases, color = drug_class, group = drug_class)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_color_manual(values = drug_colors, name = "Drug class") +
  scale_x_continuous(
    breaks = seq(min(dtA$year), max(dtA$year), by = 2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si("symbol")),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Annual JADER reports by immunosuppressant class",
    x = "Year",
    y = "Reports (report-level)"
  ) +
  theme_pub(base_size = 10) +
  theme(legend.position = "right")

print(p5a)

ggsave(file.path(outdir, "Figure5A_JADER_Annual_by_Class.pdf"),
       p5a, width = 180, height = 120, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure5A_JADER_Annual_by_Class.png"),
       p5a, width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

## =========================================================
## Figure 5B — Sex distribution by class (stacked %)
## =========================================================
dtB <- read_csv(sex_file, show_col_types = FALSE)

## QC-first: accept sex or sex3 column, must have n + total
if (all(c("drug_class","sex","n","total") %in% names(dtB))) {
  dtB <- dtB %>% rename(sex3 = sex)
} else if (all(c("drug_class","sex3","n","total") %in% names(dtB))) {
  # already sex3
} else {
  stop("Figure5B QC fail: sex file must contain columns drug_class + (sex or sex3) + n + total.")
}

dtB <- dtB %>%
  mutate(
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    sex3 = factor(as.character(sex3), levels = sex_levels),
    n = suppressWarnings(as.numeric(n)),
    total = suppressWarnings(as.numeric(total))
  )

if (anyNA(dtB$drug_class) || anyNA(dtB$sex3)) stop("Figure5B QC fail: NA in drug_class/sex after coercion.")
if (anyNA(dtB$n) || anyNA(dtB$total)) stop("Figure5B QC fail: NA in n/total after coercion.")
if (any(dtB$total <= 0)) stop("Figure5B QC fail: total <= 0 exists.")

## complete missing combinations (same as Fig2C)
dtB <- dtB %>%
  tidyr::complete(
    drug_class = factor(drug_levels, levels = drug_levels),
    sex3 = factor(sex_levels, levels = sex_levels),
    fill = list(n = 0)
  ) %>%
  group_by(drug_class) %>%
  mutate(total = max(total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    pct = ifelse(total > 0, 100 * n / total, NA_real_)
  )

if (anyNA(dtB$pct)) stop("Figure5B QC fail: pct NA after recomputation.")

## label like Fig2C (avoid clutter)
dtB <- dtB %>%
  mutate(label = ifelse(pct >= 4, paste0(round(pct, 1), "%"), ""))

p5b <- ggplot(dtB, aes(x = drug_class, y = pct, fill = sex3)) +
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
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Sex distribution of JADER reports by immunosuppressant class",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pub_box(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p5b)

ggsave(file.path(outdir, "Figure5B_JADER_Sex_Distribution_StackedPct.pdf"),
       p5b, width = 180, height = 120, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure5B_JADER_Sex_Distribution_StackedPct.png"),
       p5b, width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

## =========================================================
## Figure 5C — Age distribution by class (Median bar + IQR)
## =========================================================
dtC <- read_csv(age_file, show_col_types = FALSE)

reqC <- c("drug_class","n_nonmissing","median","q1","q3")
missC <- setdiff(reqC, names(dtC))
if (length(missC) > 0) stop("Figure5C QC fail: missing columns: ", paste(missC, collapse = ", "))

dtC <- dtC %>%
  mutate(
    drug_class = factor(as.character(drug_class), levels = drug_levels),
    median = as.numeric(median),
    q1 = as.numeric(q1),
    q3 = as.numeric(q3),
    n_nonmissing = suppressWarnings(as.integer(n_nonmissing))
  )

if (anyNA(dtC$median) || anyNA(dtC$q1) || anyNA(dtC$q3)) {
  stop("Figure5C QC fail: NA exists in median/q1/q3 after coercion.")
}
bad_iqr <- dtC %>% filter(!(q1 <= median & median <= q3))
if (nrow(bad_iqr) > 0) stop("Figure5C QC fail: found rows where q1 <= median <= q3 is violated.")

p5c <- ggplot(dtC, aes(x = drug_class, y = median, fill = drug_class)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = q1, ymax = q3),
                width = 0.18, linewidth = 0.9, color = "black") +
  geom_text(
    aes(label = paste0("Median = ", median)),
    color = "white",
    fontface = "bold",
    size = 3.2,
    vjust = 1.6
  ) +
  scale_fill_manual(values = drug_colors) +
  labs(
    title = "Age distribution of JADER reports by immunosuppressant class",
    x = NULL,
    y = "Age (years)"
  ) +
  theme_pub_box(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none")

print(p5c)

ggsave(file.path(outdir, "Figure5C_JADER_Age_MedianBar_IQR.pdf"),
       p5c, width = 180, height = 120, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure5C_JADER_Age_MedianBar_IQR.png"),
       p5c, width = 180, height = 120, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved Figure 5 panels to: ", outdir)

