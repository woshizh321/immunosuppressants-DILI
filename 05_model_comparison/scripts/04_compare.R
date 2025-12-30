# ============================================================
# Step3 — OR-level robustness comparison (FINAL, frozen)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})

cat("Project root:", here(), "\n")

# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------
path_lasso <- here(
  "03_model_P_LASSO", "outputs", "model",
  "modelP_lambda_1se_coef.csv"
)

path_logistic <- here(
  "04_model_E_logistic", "outputs", "evaluation",
  "OR_CI.csv"
)

stopifnot(file.exists(path_lasso))
stopifnot(file.exists(path_logistic))

# ------------------------------------------------------------
# Read & standardize Model-P (LASSO)
# ------------------------------------------------------------
lasso <- read_csv(path_lasso, show_col_types = FALSE)

# Standardize column names
if (!"term" %in% names(lasso)) names(lasso)[1] <- "term"
if (!"OR" %in% names(lasso)) {
  # fallback if only beta exists
  if ("beta" %in% names(lasso)) {
    lasso <- lasso %>% mutate(OR = exp(beta))
  }
}

lasso2 <- lasso %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    OR_lasso = OR
  )

# ------------------------------------------------------------
# Read & standardize Model-E (logistic)
# ------------------------------------------------------------
logi <- read_csv(path_logistic, show_col_types = FALSE)

if (!"term" %in% names(logi)) names(logi)[1] <- "term"

logi2 <- logi %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    OR_logistic = OR,
    CI_low,
    CI_high
  )

# ------------------------------------------------------------
# Merge & compare
# ------------------------------------------------------------
cmp <- full_join(lasso2, logi2, by = "term") %>%
  mutate(
    direction_match = sign(log(OR_lasso)) == sign(log(OR_logistic)),
    rank_lasso = rank(-OR_lasso, ties.method = "average"),
    rank_logistic = rank(-OR_logistic, ties.method = "average")
  ) %>%
  arrange(rank_logistic)

# ------------------------------------------------------------
# Output
# ------------------------------------------------------------
out_dir <- here("05_model_comparison", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "OR_comparison_clean.csv")
write_csv(cmp, out_file)

cat("Saved:", out_file, "\n")
print(cmp)
