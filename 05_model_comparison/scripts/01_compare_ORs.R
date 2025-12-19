suppressPackageStartupMessages({
  library(here)
})

cat("Project root detected by here():\n", here(), "\n")

library(dplyr)
library(readr)

out_dir <- "04_model_comparison/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------- inputs --------
lasso_coef <- read_csv(
  "02_model_P_LASSO/outputs/model/ModelP_lambda_1se_coefficients.csv",
  show_col_types = FALSE
)

logistic_or <- list.files(
  "04_logistic/outputs",
  pattern = "FAERS_logistic_fullmodel_OR_CI_.*\\.csv",
  full.names = TRUE
) |> sort() |> tail(1) |> read_csv(show_col_types = FALSE)

# -------- tidy --------
lasso_or <- lasso_coef |>
  filter(term != "(Intercept)") |>
  transmute(
    term,
    OR_lasso = OR
  )

logistic_or2 <- logistic_or |>
  filter(term != "(Intercept)") |>
  transmute(
    term,
    OR_logistic = OR,
    CI_low,
    CI_high
  )

# -------- merge --------
cmp <- full_join(lasso_or, logistic_or2, by = "term") |>
  mutate(
    direction_match = sign(log(OR_lasso)) == sign(log(OR_logistic))
  ) |>
  arrange(desc(OR_logistic))

write_csv(cmp, file.path(out_dir, "OR_comparison.csv"))
print(cmp)
