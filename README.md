# Immunosuppressants-associated DILI (FAERS) - Sister Project

This repository mirrors the DIKI pipeline with the outcome switched to DILI.
Current scope: hierarchical screening at SOC/HLGT/HLT levels under SOC = Hepatobiliary disorders.

## Quick start
1) Configure `00_project/LockedSpec/LockedDILI_HierarchicalSpec_v1.yml`
2) Run QC: `Rscript 02_hierarchical_signal_detection/scripts/00_qc_inputs.R`
3) Run SOC/HLGT/HLT: scripts `01_*`, `02_*`, `03_*`
4) Check docs: `02_hierarchical_signal_detection/docs/`

