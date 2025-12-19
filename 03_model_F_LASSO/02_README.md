# Penalized Regression (LASSO)

This module performs multivariable penalized logistic regression to assess
**independent risk factors** for immunosuppressant-associated DILI.

## Objective
To evaluate whether drug classes remain associated with DILI after adjustment for:
- Sex
- Age group

This analysis complements signal detection by providing a **risk modeling perspective**.

## Outcome
- Binary DILI (SOC = Hepatobiliary disorders)

## Predictors
- Sex: Male / Female / Other
- Age groups: <18, 18–39, 40–64, ≥65
- Drug class exposure (PS/SS)

## Methodology
- Case-level feature matrix
- One-hot encoding
- Logistic LASSO (glmnet)
- 10-fold cross-validation
- λ_min and λ_1se both reported

## Outputs
- Selected variables with coefficients and ORs
- Saved cv.glmnet objects
- Case-level predicted probabilities

No AE terms are included to prevent label leakage.
