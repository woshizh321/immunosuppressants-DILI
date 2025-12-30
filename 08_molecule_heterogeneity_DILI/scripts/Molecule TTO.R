## =========================================================
## Figure 6C — TTO by molecule (Data Only, NO plotting)
## TTO field locked: tto_onset_days_psss (PS/SS consistent)
## =========================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(here)
  library(readr)
})

## ---------------------------------------------------------
## 0) DuckDB connection
## ---------------------------------------------------------
duckdb_dir <- here("duckdb")
if (!dir.exists(duckdb_dir)) dir.create(duckdb_dir, recursive = TRUE)
duckdb_file <- file.path(duckdb_dir, "FAERS_Figure6C_TTO.duckdb")

con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file)
dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='20GB';")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

## ---------------------------------------------------------
## 1) MASTER parquet (with TTO)
## ---------------------------------------------------------
faers_parquet <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_TTO_ALL.parquet"
soc_target <- "Hepatobiliary disorders"

## Locked TTO column
tto_col_exact <- "tto_onset_days_psss"

## ---------------------------------------------------------
## 2) Case-level DILI flag (SOC-based)
## ---------------------------------------------------------
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_dili AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(CASE WHEN soc_name = '%s' THEN 1 ELSE 0 END) AS DILI
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid
", soc_target, faers_parquet))

## ---------------------------------------------------------
## 3) PS/SS drug rows + TTO (already computed)
## ---------------------------------------------------------
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE drug_psss_tto AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname_u)) AS drugname,
    TRY_CAST(%s AS DOUBLE)  AS tto_days
  FROM read_parquet('%s')
  WHERE role_cod_u IN ('PS','SS')
    AND caseid IS NOT NULL
    AND drugname_u IS NOT NULL
    AND %s IS NOT NULL
", tto_col_exact, faers_parquet, tto_col_exact))

## ---------------------------------------------------------
## 4) Molecule-specific case-level TTO
##    Rule: per case & molecule, MIN positive TTO (earliest)
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE mol_case_tto AS
  WITH base AS (
    SELECT caseid, drugname, tto_days
    FROM drug_psss_tto
    WHERE tto_days IS NOT NULL AND tto_days > 0
  )
  SELECT
    caseid,
    CASE
      WHEN drugname IN ('TACROLIMUS','FK506','PROGRAF','ADVAGRAF') THEN 'Tacrolimus'
      WHEN drugname IN ('BASILIXIMAB','SIMULECT') THEN 'Basiliximab'
      WHEN drugname IN ('AZATHIOPRINE','IMURAN') THEN 'Azathioprine'
      WHEN drugname IN ('EVEROLIMUS','AFINITOR','CERTICAN') THEN 'Everolimus'
      ELSE NULL
    END AS molecule,
    MIN(tto_days) AS tto_days
  FROM base
  WHERE drugname IN (
    'TACROLIMUS','FK506','PROGRAF','ADVAGRAF',
    'BASILIXIMAB','SIMULECT',
    'AZATHIOPRINE','IMURAN',
    'EVEROLIMUS','AFINITOR','CERTICAN'
  )
  GROUP BY caseid, molecule
")

dbExecute(con, "DELETE FROM mol_case_tto WHERE molecule IS NULL")

## ---------------------------------------------------------
## 5) Restrict to DILI cases (DILI=1)
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE mol_case_tto_dili AS
  SELECT
    m.caseid,
    m.molecule,
    m.tto_days
  FROM mol_case_tto m
  INNER JOIN case_dili d
    ON m.caseid = d.caseid
  WHERE d.DILI = 1
")

## ---------------------------------------------------------
## 6) Outlier filter: molecule-specific p01–p99
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE mol_tto_bounds AS
  SELECT
    molecule,
    quantile_cont(tto_days, 0.01) AS p01,
    quantile_cont(tto_days, 0.99) AS p99,
    COUNT(*) AS n_raw
  FROM mol_case_tto_dili
  GROUP BY molecule
")

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE mol_case_tto_dili_qc AS
  SELECT
    t.caseid,
    t.molecule,
    t.tto_days
  FROM mol_case_tto_dili t
  INNER JOIN mol_tto_bounds b
    ON t.molecule = b.molecule
  WHERE t.tto_days BETWEEN b.p01 AND b.p99
")

## ---------------------------------------------------------
## 7) Onset pattern bins (locked thresholds)
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE mol_case_tto_dili_qc2 AS
  SELECT
    caseid,
    molecule,
    tto_days,
    CASE
      WHEN tto_days <= 30 THEN 'Early'
      WHEN tto_days <= 180 THEN 'Intermediate'
      ELSE 'Late'
    END AS onset_pattern
  FROM mol_case_tto_dili_qc
")

## ---------------------------------------------------------
## 8) Export
## ---------------------------------------------------------
out_dir <- here("06_molecule_heterogeneity", "derived")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tto_case <- dbGetQuery(con, "SELECT * FROM mol_case_tto_dili_qc2")
write_csv(tto_case, file.path(out_dir, "Figure6C_TTO_psss_caselevel.csv"))

tto_summary <- dbGetQuery(con, "
  SELECT
    molecule,
    COUNT(*) AS n_used,
    quantile_cont(tto_days, 0.50) AS tto_median,
    quantile_cont(tto_days, 0.25) AS tto_q25,
    quantile_cont(tto_days, 0.75) AS tto_q75,
    AVG(CASE WHEN onset_pattern='Early' THEN 1 ELSE 0 END)*100 AS pct_early,
    AVG(CASE WHEN onset_pattern='Intermediate' THEN 1 ELSE 0 END)*100 AS pct_intermediate,
    AVG(CASE WHEN onset_pattern='Late' THEN 1 ELSE 0 END)*100 AS pct_late
  FROM mol_case_tto_dili_qc2
  GROUP BY molecule
  ORDER BY molecule
")
write_csv(tto_summary, file.path(out_dir, "Figure6C_TTO_psss_summary.csv"))

bounds <- dbGetQuery(con, "SELECT * FROM mol_tto_bounds ORDER BY molecule")
write_csv(bounds, file.path(out_dir, "Figure6C_TTO_psss_bounds_p01_p99.csv"))

cat("\nSaved (PS/SS TTO):\n",
    "- Figure6C_TTO_psss_caselevel.csv\n",
    "- Figure6C_TTO_psss_summary.csv\n",
    "- Figure6C_TTO_psss_bounds_p01_p99.csv\n",
    "to: ", out_dir, "\n", sep = "")

library(tidyverse)
library(here)

tto_df <- read_csv(
  here("06_molecule_heterogeneity", "derived",
       "Figure6C_TTO_psss_caselevel.csv")
)

tto_df$molecule <- factor(
  tto_df$molecule,
  levels = c("Basiliximab", "Everolimus", "Tacrolimus", "Azathioprine")
)
mol_cols <- c(
  "Basiliximab"   = "#8FA3A8",
  "Everolimus"   = "#B6A57A",
  "Tacrolimus"   = "#8E3232",
  "Azathioprine" = "#6E7B6A"
)
p6c_main <- ggplot(tto_df, aes(x = molecule, y = tto_days, fill = molecule)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,
    alpha = 0.9
  ) +
  scale_y_log10(
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1000")
  ) +
  scale_fill_manual(values = mol_cols) +
  labs(
    x = NULL,
    y = "Time to onset (days, log scale)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

p6c_main
out_dir <- here("figures", "Figure6")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(out_dir, "Figure6C_TTO_boxplot.pdf"),
  plot = p6c_main,
  width = 5.5, height = 4.5, device = cairo_pdf
)

ggsave(
  filename = file.path(out_dir, "Figure6C_TTO_boxplot.png"),
  plot = p6c_main,
  width = 5.5, height = 4.5, dpi = 300
)

