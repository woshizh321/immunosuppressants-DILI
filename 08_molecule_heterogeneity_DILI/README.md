# Step6 — Molecule heterogeneity (DILI)

## Goal
Quantify within-class molecule-level heterogeneity of DILI risk under regimen-aware adjustment,
using FAERS MASTER parquet via DuckDB (disk-based, no arrow).

## Inputs
- FAERS MASTER parquet:
  D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet
- Drug class dictionary (locked, shared):
  99_shared/dict/immunosuppressants_drug_classes.R

## Outcome (DILI)
Default: DILI = 1 if soc_name == "Hepatobiliary disorders" else 0.
(PT-level is NOT required here; PT signals are supplementary only.)

## Modeling strategy
Two complementary analyses:
A) Within-class restricted model:
   For each class (e.g., CNI), restrict to reports exposed to that class, then model
   DILI ~ molecule indicators + sex_bin + age_bin + co-exposure to other classes (regimen adjustment).
B) Global model:
   Use all reports and include all molecule indicators across classes, plus regimen adjustment.

Primary estimator: logistic regression (glm) with robust SE or standard SE (configurable).
(If separation occurs, switch to Firth/logistf — optional extension.)

## Outputs
- QC: schema snapshot, counts, exposure coverage, molecule mapping coverage
- case-level table (minimal columns) written as RDS
- molecule-level OR/CI tables (within-class + global)
- figures (optional forest plots)

## How to run
From RStudio at project root:
source("08_molecule_heterogeneity_DILI/scripts/00_run_all.R")

## Notes
- DuckDB reads parquet on disk; do not load full parquet into memory.
- All derived variables (sex_bin, age_bin, DILI) are generated inside DuckDB queries.
