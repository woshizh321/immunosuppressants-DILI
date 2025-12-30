## =========================================================
## Figure 4B — Model_E (logistic) | Forest plot of OR with 95% CI
## Input: OR_CI.csv
## Output: PDF + PNG
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

infile <- "D:/Projects/immunosuppressants_DILI/04_model_E_logistic/outputs/evaluation/OR_CI.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure4"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## ---- Locked colors (same as Figure 3) ----
plot_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "mTOR inhibitors"          = "#B08B6E",
  "Biologics"                = "#8A7F9B",
  "Population"               = "#6E6E6E"
)

## ---- Read + QC ----
dt0 <- read_csv(infile, show_col_types = FALSE)
req <- c("term","OR","CI_low","CI_high")
miss <- setdiff(req, names(dt0))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

dt <- dt0 %>%
  mutate(
    term = as.character(term),
    OR = suppressWarnings(as.numeric(OR)),
    CI_low = suppressWarnings(as.numeric(CI_low)),
    CI_high = suppressWarnings(as.numeric(CI_high))
  ) %>%
  filter(!str_detect(tolower(term), "intercept"))

if (nrow(dt) == 0) stop("No non-intercept terms to plot.")
if (anyNA(dt$OR) | anyNA(dt$CI_low) | anyNA(dt$CI_high)) stop("QC fail: OR/CI contains NA.")
if (any(dt$OR <= 0) | any(dt$CI_low <= 0) | any(dt$CI_high <= 0)) stop("QC fail: OR/CI <= 0 found.")
if (any(dt$CI_low > dt$CI_high)) stop("QC fail: CI_low > CI_high found.")

## ---- Fixed mapping: term -> color group ----
dt <- dt %>%
  mutate(
    color_key = case_when(
      term == "flag_calcineurin_inhibitors"   ~ "Calcineurin inhibitors",
      term == "flag_antiproliferative_agents" ~ "Antiproliferative agents",
      term == "flag_corticosteroids"          ~ "Corticosteroids",
      term == "flag_mtor_inhibitors"          ~ "mTOR inhibitors",
      term == "flag_biologics"                ~ "Biologics",
      TRUE                                    ~ "Population"
    ),
    term_label = case_when(
      term == "flag_calcineurin_inhibitors"   ~ "Calcineurin inhibitors",
      term == "flag_antiproliferative_agents" ~ "Antiproliferative agents",
      term == "flag_mtor_inhibitors"          ~ "mTOR inhibitors",
      term == "flag_corticosteroids"          ~ "Corticosteroids",
      term == "flag_biologics"                ~ "Biologics",
      term == "sex_binMale"                   ~ "Male",
      term == "sex_binOther"                  ~ "Other sex",
      term == "age_bin65+"                    ~ "Age ≥65",
      term == "age_bin<18"                    ~ "Age <18",
      term == "age_bin40-64"                  ~ "Age 40–64",
      TRUE ~ term
    )
  )

## ---- Ordering: show drug-class first (by OR), then population terms (by OR) ----
dt <- dt %>%
  mutate(is_drug = color_key != "Population") %>%
  arrange(desc(is_drug), desc(OR)) %>%
  mutate(term_f = factor(term_label, levels = rev(unique(term_label))))

## ---- Theme ----
theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size - 1, color = "black"),
      legend.title = element_blank(),
      legend.text  = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## ---- Plot ----
point_size <- 5  # ✅ bigger circles

p4B <- ggplot(dt, aes(x = OR, y = term_f)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high, color = color_key),
    height = 0.20, linewidth = 0.9
  ) +
  geom_point(aes(color = color_key), size = point_size) +
  scale_color_manual(values = plot_colors, name = NULL) +
  scale_x_log10(
    breaks = c(0.5, 1, 1.5, 2, 3),
    labels = c("0.5","1","1.5","2","3"),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  labs(
    title = "Model_E (logistic): crude associations with DILI",
    x = "Odds ratio (log scale)",
    y = NULL
  ) +
  theme_pub_box(base_size = 10)

print(p4B)

## ---- Export ----
ggsave(file.path(outdir, "Figure4B_ModelE_Logistic_Forest_OR.pdf"),
       p4B, width = 190, height = 190, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure4B_ModelE_Logistic_Forest_OR.png"),
       p4B, width = 190, height = 190, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

