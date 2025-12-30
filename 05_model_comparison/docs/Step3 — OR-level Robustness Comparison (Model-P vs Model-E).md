# Step3 — OR-level Robustness Comparison (Model-P vs Model-E)

## Objective
To formally assess robustness of drug–DILI associations by comparing
effect estimates derived from:
- Model-P: Penalized logistic regression (LASSO, lambda.1se)
- Model-E: Full multivariable logistic regression

This step does NOT re-fit models. It standardizes and contrasts
already-derived effect estimates to verify:
- Directional consistency
- Rank stability
- Absence of penalization-induced artifacts

## Data Sources
- Model-P coefficients:
  03_model_P_LASSO/outputs/model/modelP_lambda_1se_coef.csv
- Model-E OR/CI:
  04_model_E_logistic/outputs/evaluation/OR_CI.csv

## Outputs
- OR_comparison_clean.csv:
  Harmonized comparison table suitable for manuscript tables
  and supplementary materials.

## Interpretation Guide
- Direction match = TRUE indicates consistent risk direction.
- Similar ranking across models supports structural robustness.
- Minor magnitude differences are expected due to shrinkage.
