
# 03_model_P_LASSO — Prediction-oriented LASSO model

This module implements the main prediction-oriented LASSO (Model-P),
using fixed specifications defined in 00_project/LockedSpec.
Outputs from this module are used for downstream model comparison
and external validation (JADER).

## Purpose
This step performs a **standard multivariable logistic regression (unpenalized)** as a sensitivity analysis to verify that the major conclusions from LASSO are not artifacts of penalization.

It answers:
- Do drug-class effects remain directionally consistent?
- Are effect sizes in the same order of magnitude?
- Are demographic associations (sex/age) reasonable?

## Outcome Definition
- **DILI (binary)** at case level:
  - DILI=1 if the case has ≥1 AE record with **SOC = "Hepatobiliary disorders"**
  - DILI=0 otherwise

## Predictors (fixed)
- Sex: Male / Female / Other (from `sex_std`)
- Age bins: <18, 18–39, 40–64, 65+ (from `age_group` mapping)
- Drug exposure (PS/SS): 5 immunosuppressant classes (one-hot flags)

## Data Source
- FAERS MASTER parquet:
  - `D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet`

## Key Rules
- Case-level aggregation (one row per `caseid`)
- Exposure is defined using `role_cod IN ('PS','SS')`
- Do NOT include AE terms (PT/HLT/HLGT) as predictors (prevents label leakage)
- Do NOT include seriousness variables in Model-E (potential downstream/collider)

## Outputs
Saved under `04_logistic/outputs/`:
1. `FAERS_logistic_fullmodel_OR_CI_<timestamp>.csv`
   - term, beta, SE, OR, CI_low, CI_high, p_value, N_used, N_DILI
2. `FAERS_logistic_fullmodel_model_<timestamp>.rds`
   - fitted glm object
3. `FAERS_logistic_fullmodel_design_matrix_cols_<timestamp>.csv`
   - records the final encoded columns for traceability

## Acceptance Criteria
- Drug-class ORs show a reasonable gradient similar to LASSO (minor shrinkage OK)
- No major sign flips for key classes (especially CNI and APA)
- Sex and age effects are directionally plausible
- Sample counts (N_used, N_DILI) are reported and non-zero

## Notes
- This model is **not** intended to replace LASSO Model-P.
- Model-E is for robustness validation and interpretable OR/CI reporting.
