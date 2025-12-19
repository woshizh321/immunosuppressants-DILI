# Step3 — Model-P vs Model-E Comparison (FAERS internal validation)

## Purpose
This step compares the penalized model (Model-P, LASSO) and the unpenalized model
(Model-E, full logistic regression) to demonstrate robustness of inference.

The goal is NOT to optimize performance, but to verify:
- Directional consistency of effects
- Stability of effect size ordering
- Comparable calibration behavior
- No evidence of penalization-induced artifacts

## Models Compared
- Model-P: LASSO logistic (lambda.1se as primary)
- Model-E: Full multivariable logistic regression

## Comparison Dimensions
1. Effect estimates (OR-level comparison)
2. Calibration behavior (observed vs predicted)
3. Global metrics (AUROC, AUPRC, Brier)

## Key Principle
If conclusions remain consistent across Model-P and Model-E,
the observed drug–DILI associations are considered structurally robust.

## Outputs
- OR_comparison.csv
- calibration_comparison.csv
- metrics_comparison.csv

These outputs serve as the internal validation backbone
before external validation using JADER.
