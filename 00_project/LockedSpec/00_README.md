# Locked Specifications

This directory contains **locked and versioned specifications** that define the analytical scope of the project.

Once finalized, these files **must not be modified** without explicit versioning, as they ensure consistency across:
- Signal detection
- Validation analyses
- Predictive modeling (LASSO)

## Contents

### 1. Drug Definitions
- Immunosuppressant drug dictionaries
- Mapping from raw FAERS drug names to standardized drug classes:
  - Calcineurin inhibitors
  - mTOR inhibitors
  - Antiproliferative agents
  - Corticosteroids
  - Biologics

### 2. Outcome Definition
- DILI defined strictly at **SOC level**
- SOC = "Hepatobiliary disorders"

### 3. Population Rules
- Case-level aggregation
- Drug exposure defined by PS/SS role codes
- No filtering by seriousness or report source

Any downstream analysis must adhere to these locked specifications.
