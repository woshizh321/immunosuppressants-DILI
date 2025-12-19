suppressPackageStartupMessages({
  library(here)
})

cat("Project root detected by here():\n", here(), "\n")


library(dplyr)
library(readr)

out_dir <- "04_model_comparison/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------- inputs --------
cal_lasso <- read_csv(
  "02_model_P_LASSO/outputs/evaluation/calibration_table_lambda_1se.csv",
  show_col_types = FALSE
) |>
  mutate(model = "LASSO")

cal_logistic <- read_csv(
  "04_logistic/outputs/calibration_table.csv",
  show_col_types = FALSE
) |>
  mutate(model = "Logistic")

# -------- combine --------
cal_cmp <- bind_rows(cal_lasso, cal_logistic) |>
  select(model, n, obs, pred)

write_csv(cal_cmp, file.path(out_dir, "calibration_comparison.csv"))
print(head(cal_cmp, 10))
