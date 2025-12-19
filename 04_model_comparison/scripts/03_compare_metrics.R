suppressPackageStartupMessages({
  library(here)
})

cat("Project root detected by here():\n", here(), "\n")

library(dplyr)
library(readr)

out_dir <- "04_model_comparison/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------- inputs --------
metrics_lasso <- read_csv(
  "02_model_P_LASSO/outputs/evaluation/metrics_summary.csv",
  show_col_types = FALSE
) |>
  mutate(model = paste0("LASSO_", model))

metrics_logistic <- read_csv(
  "04_logistic/outputs/metrics_summary.csv",
  show_col_types = FALSE
) |>
  mutate(model = "Logistic")

# -------- combine --------
metrics_cmp <- bind_rows(metrics_lasso, metrics_logistic)

write_csv(metrics_cmp, file.path(out_dir, "metrics_comparison.csv"))
print(metrics_cmp)
