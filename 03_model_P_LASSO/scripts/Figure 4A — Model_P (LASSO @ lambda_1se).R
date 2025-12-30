## =========================================================
## Figure 4A — Model_P (LASSO @ lambda_1se) | Forest plot of OR
## Input: modelP_lambda_1se_coef.csv
## Output: PDF + PNG
## =========================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

## -------------------- 1) Paths --------------------
infile <- "D:/Projects/immunosuppressants_DILI/03_model_P_LASSO/outputs/model/modelP_lambda_1se_coef.csv"
outdir <- "D:/Projects/immunosuppressants_DILI/10_figures/Figure4"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

## -------------------- 2) Colors (locked) --------------------
## Drug-class colors (kept for consistency, even if Model_P typically has none)
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
message("Columns in modelP_lambda_1se_coef.csv:\n", paste(names(dt0), collapse = ", "))

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

col_term <- pick_col(dt0, c("term","variable","feature","name"))
col_or   <- pick_col(dt0, c("OR","or"))
col_lo   <- pick_col(dt0, c("CI_low","ci_low","lower","OR_L","ROR_L"))
col_hi   <- pick_col(dt0, c("CI_high","ci_high","upper","OR_U","ROR_U"))
col_beta <- pick_col(dt0, c("beta","coef","coefficient"))
col_lam  <- pick_col(dt0, c("lambda","lambda_type","which_lambda","set"))

need <- c(col_term, col_or)
if (any(is.na(need))) stop("QC fail: need at least term + OR columns.")

dt <- dt0 %>%
  transmute(
    term = as.character(.data[[col_term]]),
    OR = suppressWarnings(as.numeric(.data[[col_or]])),
    CI_low  = if (!is.na(col_lo)) suppressWarnings(as.numeric(.data[[col_lo]])) else NA_real_,
    CI_high = if (!is.na(col_hi)) suppressWarnings(as.numeric(.data[[col_hi]])) else NA_real_,
    beta = if (!is.na(col_beta)) suppressWarnings(as.numeric(.data[[col_beta]])) else NA_real_,
    lambda = if (!is.na(col_lam)) as.character(.data[[col_lam]]) else "1se"
  )

## Filter to lambda=1se if lambda column exists
dt <- dt %>% filter(tolower(lambda) %in% c("1se","lambda_1se","lam1se","one_se","one-se","1se." ) | is.na(lambda) | lambda=="1se")

if (nrow(dt) == 0) stop("No rows after filtering lambda to 1se. Check lambda column values.")
if (anyNA(dt$OR)) stop("QC fail: OR has NA after coercion.")
if (any(dt$OR <= 0)) stop("QC fail: OR <= 0 found; cannot plot on log scale.")

has_ci <- !(all(is.na(dt$CI_low)) | all(is.na(dt$CI_high)))
if (has_ci) {
  if (any(dt$CI_low <= 0, na.rm = TRUE) || any(dt$CI_high <= 0, na.rm = TRUE)) stop("QC fail: CI bounds <=0.")
  if (any(dt$CI_low > dt$CI_high, na.rm = TRUE)) stop("QC fail: CI_low > CI_high.")
}

## Remove intercept if present
dt <- dt %>% filter(!str_detect(tolower(term), "intercept"))

## -------------------- 4) Fixed mapping: Model_P uses Population color --------------------
## (If any drug flags appear unexpectedly, color them consistently)
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

## -------------------- 5) Ordering (|beta| else |log(OR)|) --------------------
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
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = margin(8, 10, 8, 8)
    )
}

## -------------------- 7) Plot --------------------
point_size <- 4.4  # ✅ bigger circles

p4A <- ggplot(dt, aes(x = OR, y = term_f)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6, color = "grey35") +
  {if (has_ci) geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high, color = color_key),
    height = 0.20, linewidth = 0.85, na.rm = TRUE
  )} +
  geom_point(aes(color = color_key), size = point_size) +
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 5, 10, 20, 50),
    labels = c("0.25","0.5","1","2","5","10","20","50"),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_color_manual(values = plot_colors, guide = "none") +
  labs(
    title = "Model_P (LASSO @ lambda_1se): population-level associations with DILI",
    x = "Odds ratio (log scale)",
    y = NULL
  ) +
  theme_pub_box(base_size = 10)

print(p4A)

## -------------------- 8) Export --------------------
ggsave(file.path(outdir, "Figure4A_ModelP_LASSO_lambda1se_Forest_OR.pdf"),
       p4A, width = 190, height = 200, units = "mm", device = cairo_pdf)
ggsave(file.path(outdir, "Figure4A_ModelP_LASSO_lambda1se_Forest_OR.png"),
       p4A, width = 190, height = 200, units = "mm",
       dpi = 300, device = ragg::agg_png, bg = "white")

message("Saved:\n- ", file.path(outdir, "Figure4A_ModelP_LASSO_lambda1se_Forest_OR.pdf"),
        "\n- ", file.path(outdir, "Figure4A_ModelP_LASSO_lambda1se_Forest_OR.png"))
