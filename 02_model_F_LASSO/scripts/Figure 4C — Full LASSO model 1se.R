## =========================================================
## Figure 4C — Model_full (LASSO, nonzero @ lambda_1se) | Forest plot of OR
## Input: nonzero_lambda_1se.csv
## Output: PDF + PNG
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(forcats)
  library(scales)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/02_model_F_LASSO/outputs/model/nonzero_lambda_1se.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure4"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Locked drug colors (same as Figure 3) --------------------
drug_levels <- c(
  "Calcineurin inhibitors",
  "mTOR inhibitors",
  "Antiproliferative agents",
  "Corticosteroids",
  "Biologics"
)

plot_colors <- c(
  "Calcineurin inhibitors"   = "#6D8FA6",
  "Antiproliferative agents" = "#7C8A6A",
  "Corticosteroids"          = "#A77A8B",
  "mTOR inhibitors"          = "#B08B6E",
  "Biologics"                = "#8A7F9B",
  "Population"               = "#6E6E6E"
)


## -------------------- 3) Read + schema QC --------------------
dt0 <- read_csv(infile, show_col_types = FALSE)
message("Columns in nonzero_lambda_1se.csv:\n", paste(names(dt0), collapse = ", "))

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

col_term <- pick_col(dt0, c("term","variable","feature","name"))
col_or   <- pick_col(dt0, c("OR","or"))
col_lo   <- pick_col(dt0, c("CI_low","ci_low","lower","OR_L","ROR_L"))
col_hi   <- pick_col(dt0, c("CI_high","ci_high","upper","OR_U","ROR_U"))
col_beta <- pick_col(dt0, c("beta","coef","coefficient"))

need <- c(col_term, col_or)
if (any(is.na(need))) {
  stop("QC fail: Cannot find required columns. Need at least term + OR.\n",
       "Detected: term=", col_term, "; OR=", col_or)
}

dt <- dt %>%
  mutate(
    color_key = case_when(
      term == "flag_calcineurin_inhibitors"   ~ "Calcineurin inhibitors",
      term == "flag_antiproliferative_agents" ~ "Antiproliferative agents",
      term == "flag_corticosteroids"          ~ "Corticosteroids",
      term == "flag_mtor_inhibitors"          ~ "mTOR inhibitors",
      term == "flag_biologics"                ~ "Biologics",
      TRUE                                    ~ "Population"
    )
  )


if (anyNA(dt$OR)) stop("QC fail: OR contains NA after numeric coercion.")
if (any(dt$OR <= 0)) stop("QC fail: OR <= 0 found; cannot plot on log scale.")

has_ci <- !(all(is.na(dt$CI_low)) | all(is.na(dt$CI_high)))
if (has_ci) {
  if (any(dt$CI_low <= 0, na.rm = TRUE) || any(dt$CI_high <= 0, na.rm = TRUE)) {
    stop("QC fail: CI bounds <= 0 exist.")
  }
  if (any(dt$CI_low > dt$CI_high, na.rm = TRUE)) stop("QC fail: CI_low > CI_high found.")
}

## Remove intercept if present
dt <- dt %>% filter(!str_detect(tolower(term), "intercept"))

## -------------------- 4) Annotate variable types + drug class mapping --------------------
dt <- dt %>%
  mutate(
    drug_class = case_when(
      str_detect(tolower(term), "calcineurin") ~ "Calcineurin inhibitors",
      str_detect(tolower(term), "\\bmtor\\b") ~ "mTOR inhibitors",
      str_detect(tolower(term), "antiproliferative") ~ "Antiproliferative agents",
      str_detect(tolower(term), "corticosteroid") ~ "Corticosteroids",
      str_detect(tolower(term), "biologic") ~ "Biologics",
      TRUE ~ NA_character_
    ),
    var_group = case_when(
      !is.na(drug_class) ~ "Drug class",
      str_detect(tolower(term), "sex") ~ "Sex",
      str_detect(tolower(term), "age") ~ "Age",
      TRUE ~ "Other"
    ),
    color_key = ifelse(!is.na(drug_class), drug_class, var_group)
  )

other_colors <- c("Sex" = "#6E6E6E", "Age" = "#6E6E6E", "Other" = "#6E6E6E")
plot_colors <- c(drug_colors, other_colors)

## -------------------- 5) Order terms (|beta| else |log(OR)|) --------------------
dt <- dt %>%
  mutate(rank_score = ifelse(!is.na(beta), abs(beta), abs(log(OR)))) %>%
  arrange(desc(rank_score)) %>%
  mutate(term_f = factor(term, levels = rev(unique(term))))

## -------------------- 6) Theme --------------------
theme_pub_box <- function(base_size = 10, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title   = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size - 1, color = "black"),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text  = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## -------------------- 7) Plot --------------------
point_size <- 5  # ✅ bigger circles (adjust here)

p4_full_1se <- ggplot(dt, aes(x = OR, y = term_f)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  {if (has_ci) geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = color_key),
                              height = 0.20, linewidth = 0.85, na.rm = TRUE)} +
  geom_point(aes(color = color_key), size = point_size) +
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 5, 10, 20, 50),
    labels = c("0.25","0.5","1","2","5","10","20","50"),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_color_manual(values = plot_colors, name = NULL) +
  labs(
    title = "Model_full (LASSO @ lambda_1se): adjusted associations with DILI",
    x = "Odds ratio (log scale)",
    y = NULL
  ) +
  theme_pub_box(base_size = 10)

print(p4_full_1se)

## -------------------- 8) Export --------------------
pdf_file <- file.path(outdir, "Figure4C_ModelFull_LASSO_lambda1se_Forest_OR.pdf")
png_file <- file.path(outdir, "Figure4C_ModelFull_LASSO_lambda1se_Forest_OR.png")

ggsave(pdf_file, p4_full_1se, width = 190, height = 220, units = "mm", device = cairo_pdf)
ggsave(png_file, p4_full_1se, width = 190, height = 220, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", pdf_file, "\n- ", png_file)

