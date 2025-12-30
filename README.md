# Immunosuppressant-Associated Drug-Induced Liver Injury (DILI)
FAERS-based pharmacovigilance pipeline with external validation in JADER, covering multi-level signal detection (SOC/HLGT/HLT/PT), penalized modeling (LASSO), molecule-level heterogeneity, risk portraits, and time-to-onset (TTO) characterization.

## Project highlights
- **Unified DILI definition**: MedDRA SOC = *Hepatobiliary disorders* (FAERS & JADER).
- **Multi-level signal landscape**: SOC → HLGT → HLT → PT hierarchical signal detection.
- **Modeling**: penalized logistic regression (LASSO, λ=1se) for stable feature selection; model robustness comparisons.
- **Molecule-level heterogeneity**: within-class and across-molecule risk estimates.
- **Risk portrait + TTO**: population-stratified risk portraits (sex × age × molecule) and PS/SS-consistent TTO profiles.

---

## Repository structure
> Directory names are numbered to reflect the analysis workflow.

- `00_characteristics_FAERS/`  
  Descriptive characteristics of FAERS DILI reports (e.g., annual trends, age/sex/region distributions).  
- `00_characteristics_JADER/`  
  Descriptive characteristics of JADER DILI reports (parallel summaries; region not available).
- `01_signal_detection/`  
  Hierarchical signal detection (SOC/HLGT/HLT/PT) and figure scripts.
- `02_model_F_LASSO/`  
  Full model (LASSO, λ=1se): coefficient extraction, evaluation outputs, and figure scripts.
- `03_model_P_LASSO/`  
  Parsimonious Model-P (LASSO, λ=1se) and evaluation scripts.
- `04_model_E_logistic/`  
  Conventional multivariable logistic model (Model-E) for sensitivity/robustness.
- `05_model_comparison/`  
  OR-level robustness comparison and model contrasts (Model-P vs Model-E).
- `06_external_validation_JADER/`  
  External validation in JADER: calibration, discrimination, and evaluation plots/tables.
- `07_risk_portrait/`  
  Risk portrait generation utilities and derived risk tables (code only; outputs ignored).
- `08_molecule_heterogeneity_DILI/`  
  Molecule-level heterogeneity analyses, risk portraits for selected molecules, and TTO preparation.
- `99_shared/`  
  Shared dictionaries, mappings, and reusable utilities.

---

## Data availability and privacy
This repository contains **code and documentation only**.  
All source datasets (FAERS/JADER raw or master files), intermediate data products, and large outputs are **excluded** via `.gitignore`.

To reproduce analyses, prepare local data files on your machine following the paths described below.

---

## Local data prerequisites (not tracked in Git)
### FAERS master (example)
Place the FAERS master parquet/CSV in a local folder, e.g.
- `D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet`
- `D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_TTO_ALL.parquet`

### JADER master (example)
Place the JADER master parquet/CSV in a local folder, e.g.
- `D:/JADER/MASTER/JADER_MASTER_....parquet`

> Note: Scripts use `duckdb::read_parquet()` and do not require loading full files into memory.

---

## Environment
- R (>= 4.2 recommended)
- Key R packages: `duckdb`, `DBI`, `data.table`, `dplyr`, `tidyr`, `ggplot2`, `readr`, `here`, `glmnet`  
- Optional (plot export): `Cairo` (for high-quality vector PDF)

Install packages:
```r
install.packages(c(
  "duckdb","DBI","data.table","dplyr","tidyr","ggplot2","readr","here","glmnet"
))
