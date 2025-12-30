# ============================================================
# Step4-00: Prepare JADER case-level table (FAERS-aligned)
# ============================================================

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(data.table)
  library(here)
})

library(data.table)
setDT(case_level)


cat("Project root:", here(), "\n")

# ------------------------------------------------------------
# 0. Paths & parameters (FROZEN)
# ------------------------------------------------------------

# JADER master (input)
jader_parquet <- "D:/JADER/MASTER/JADER_MASTER_PT_English_SUPERMASTER_v4_withDrugCode.parquet"

stopifnot(file.exists(jader_parquet))

# Output
out_dir  <- here("06_external_validation_JADER", "outputs", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "case_level_table_JADER.rds")


stopifnot(file.exists(drug_dict_path))
source(drug_dict_path)

cat("Loaded drug classes:\n")

# ------------------------------------------------------------
# 1. Connect DuckDB (read-only, memory-safe)
# ------------------------------------------------------------

con <- dbConnect(
  duckdb(),
  dbdir = ":memory:",
  read_only = FALSE
)

# Tune DuckDB (safe for 32G RAM)
dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='20GB';")

# ------------------------------------------------------------
# 2. Register parquet as DuckDB view
# ------------------------------------------------------------

dbExecute(con, sprintf("
  CREATE VIEW jader AS
  SELECT * FROM read_parquet('%s')
", jader_parquet))

# Quick schema QC
cat("JADER columns:\n")
print(dbListFields(con, "jader"))

case_level <- dbGetQuery(con, "
  SELECT
    CAST(ID AS VARCHAR) AS caseid,

    /* ---- Outcome: DILI (SOC Hepatobiliary disorders) ---- */
    MAX(CASE
      WHEN CAST(SOC_CODE AS VARCHAR) LIKE '10019805%' THEN 1 ELSE 0
    END) AS DILI,

    /* ---- Sex bin (FAERS-aligned) ---- */
    MAX(CASE
      WHEN SEX_GROUP IN ('Male','M')   THEN 'Male'
      WHEN SEX_GROUP IN ('Female','F') THEN 'Female'
      ELSE 'Other'
    END) AS sex_bin,

    /* ---- Age bin (FAERS-aligned; based on AGE_NUM) ---- */
    MAX(CASE
      WHEN AGE_NUM IS NULL THEN NULL
      WHEN AGE_NUM < 18 THEN '<18'
      WHEN AGE_NUM >= 18 AND AGE_NUM <= 39 THEN '18-39'
      WHEN AGE_NUM >= 40 AND AGE_NUM <= 64 THEN '40-64'
      WHEN AGE_NUM >= 65 THEN '65+'
      ELSE NULL
    END) AS age_bin,

    /* ---- Drug-class exposure flags (JADER native; case-level via MAX) ---- */
    MAX(EXPOSED_CNI)  AS flag_calcineurin_inhibitors,
    MAX(EXPOSED_MTOR) AS flag_mtor_inhibitors,
    MAX(EXPOSED_ANTI) AS flag_antiproliferative_agents,
    MAX(EXPOSED_CS)   AS flag_corticosteroids,
    MAX(EXPOSED_BIO)  AS flag_biologics

  FROM jader
  GROUP BY caseid
")



# ------------------------------------------------------------
# 5. QC-first filtering (STRICT, same as FAERS)
# ------------------------------------------------------------

cat("Before age filter:\n")
print(case_level[, list(
  N = .N,
  DILI = sum(DILI),
  rate = mean(DILI)
)])

# Drop cases with unmapped age
case_level <- case_level[!is.na(age_bin)]

cat("After age filter:\n")
print(case_level[, .(
  N = .N,
  DILI = sum(DILI),
  rate = mean(DILI)
)])

# Ensure binary flags
flag_cols <- grep("^flag_", names(case_level), value = TRUE)
for (v in flag_cols) {
  stopifnot(all(case_level[[v]] %in% c(0,1)))
}

# Factor levels (LOCKED)
case_level[, sex_bin := factor(sex_bin,
                               levels = c("Female","Male","Other"))]
case_level[, age_bin := factor(age_bin,
                               levels = c("18-39","<18","40-64","65+"))]

# ------------------------------------------------------------
# 6. Save output
# ------------------------------------------------------------

saveRDS(case_level, out_file)

cat("Saved JADER case-level table:\n", out_file, "\n")
cat("Final dimensions:\n")
print(dim(case_level))

# ------------------------------------------------------------
# 7. Disconnect
# ------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)

cat("Step4-00 completed successfully.\n")



