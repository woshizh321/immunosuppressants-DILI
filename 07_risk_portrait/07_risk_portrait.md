# Regimen-aware Risk Portraits

## Objective
To translate the externally validated, parsimonious risk structure into
clinically interpretable regimen-aware risk portraits reflecting real-world
immunosuppressive treatment patterns.

## Rationale
In routine clinical practice, immunosuppressants are prescribed as combination
regimens rather than monotherapies. Therefore, risk portraits are constructed at
the regimen (drug-class combination) level rather than at the individual drug
level.

## Inputs
- Model_E coefficients (`04_model_E_logistic/outputs/evaluation/OR_CI.csv`)
- Frozen demographic and drug-class definitions from previous steps

## Methods
- Risk scores are computed as additive sums of log(OR) contributions from:
  - sex
  - age group
  - immunosuppressive drug classes included in the regimen
- Only clinically plausible regimens are considered:
  - ≤2 core immunosuppressive classes
  - Corticosteroids allowed as background therapy

## Outputs
- `regimen_risk_portrait_table.csv`
  - sex
  - age
  - regimen composition
  - combined log(OR) and OR
  - risk category (Low / Moderate / High / Very high)

## Interpretation
Risk portraits represent relative risk configurations within commonly used
immunosuppressive regimens and should not be interpreted as individual-level
predictive estimates.
