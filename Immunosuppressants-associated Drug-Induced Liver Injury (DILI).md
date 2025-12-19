# Immunosuppressants-associated Drug-Induced Liver Injury (DILI)

This project investigates drug-induced liver injury (DILI) associated with commonly used immunosuppressants using FAERS real-world pharmacovigilance data.

The analysis follows a **hierarchical and modular workflow**, progressing from signal detection to multivariable risk modeling, with strict consistency in:
- Drug definitions
- Outcome definition (SOC-level DILI)
- Case-level aggregation
- Reproducible computation (R + DuckDB)

## Key Features
- Case-level FAERS analysis (2004–2024)
- Hierarchical signal detection (SOC → HLGT → HLT → PT)
- Penalized regression (LASSO) for independent risk assessment
- Fully modular project structure with version-controlled scripts and outputs

## Outcome Definition
- **DILI (binary)**: A case is considered DILI-positive if it contains ≥1 adverse event under  
  **SOC = "Hepatobiliary disorders"** (MedDRA).

PT/HLGT/HLT levels are used **only for signal decomposition and validation**, not for outcome definition.

## Technology Stack
- R (data.table, dplyr, glmnet)
- DuckDB (file-backed, disk-spill enabled)
- FAERS MASTER parquet files
- MedDRA v28.0

## Directory Overview
- `00_project/` – locked specifications and configuration
- `02_hierarchical_signal_detection/` – SOC/HLGT/HLT/PT signal analyses
- `04_lasso/` – multivariable penalized regression (LASSO)
- `99_shared/` – shared dictionaries, functions, and temporary DuckDB storage
