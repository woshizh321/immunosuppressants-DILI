suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
})

# -----------------------------
# 0. Paths & inputs
# -----------------------------
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"

# load drug dictionary
source("99_shared/dict/immunosuppressants_drug_classes.R")

# flatten drug list
drug_vec <- unique(toupper(unlist(drug_classes)))

role_include <- c("PS", "SS")

# -----------------------------
# 1. Connect duckDB
# -----------------------------
con <- dbConnect(duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=4;")
dbExecute(con, "PRAGMA memory_limit='8GB';")

dbExecute(con, sprintf("
  CREATE VIEW faers AS
  SELECT
    CAST(caseid AS VARCHAR)   AS caseid,
    UPPER(TRIM(drugname))     AS drugname,
    role_cod
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
", faers_path))

# -----------------------------
# 2. Register drug list
# -----------------------------
drug_tbl <- data.frame(drugname = drug_vec, stringsAsFactors = FALSE)
dbWriteTable(con, "drug_list", drug_tbl, overwrite = TRUE)

# -----------------------------
# 3. Case-level exposure flag
# -----------------------------
qc_exposure <- dbGetQuery(con, sprintf("
  WITH base AS (
    SELECT
      caseid,
      MAX(
        CASE
          WHEN role_cod IN ('PS','SS')
           AND drugname IN (SELECT drugname FROM drug_list)
          THEN 1 ELSE 0
        END
      ) AS exposed
    FROM faers
    GROUP BY caseid
  )
  SELECT
    COUNT(*)              AS n_cases_total,
    SUM(exposed)          AS n_cases_exposed,
    ROUND(1.0 * SUM(exposed) / COUNT(*), 4) AS exposed_rate
  FROM base
"))

print(qc_exposure)

