# Hierarchical Signal Detection

This module performs hierarchical pharmacovigilance signal detection for immunosuppressant-associated DILI.

Signal analyses are conducted at multiple MedDRA levels to characterize:
- Overall risk (SOC)
- Thematic clustering of events (HLGT)
- Clinical phenotype patterns (HLT)
- Fine-grained validation (PT, supplementary)

All analyses are:
- Case-level
- Based on 2×2 contingency tables
- Quantified using ROR, PRR, and χ² statistics

## Analysis Flow
1. SOC-level signal detection (primary)
2. HLGT-level decomposition (risk localization)
3. HLT-level refinement (phenotype characterization)
4. PT-level validation (supplementary only)

## Key Principles
- No PT/HLT/HLGT terms are used to define outcomes
- Hierarchical consistency is required (signal convergence)
- Disk-backed DuckDB is used to prevent memory overflow
