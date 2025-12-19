suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

# -----------------------------
# Inputs
# -----------------------------
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target <- "Hepatobiliary disorders"

source("99_shared/dict/immunosuppressants_drug_classes.R")
drug_vec <- unique(toupper(unlist(drug_classes)))

# -----------------------------
# Connect duckDB
# -----------------------------
con <- dbConnect(duckdb(), dbdir=":memory:")
on.exit(dbDisconnect(con, shutdown=TRUE), add=TRUE)

dbExecute(con, "PRAGMA threads=4;")
dbExecute(con, "PRAGMA memory_limit='8GB';")

# -----------------------------
# Base table (only needed cols)
# -----------------------------
dbExecute(con, sprintf("
  CREATE VIEW faers AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname))   AS drugname,
    role_cod,
    soc_name
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
", faers_path))

# -----------------------------
# Drug list
# -----------------------------
dbWriteTable(
  con,
  "drug_list",
  data.frame(drugname = drug_vec, stringsAsFactors = FALSE),
  overwrite = TRUE
)

# -----------------------------
# Case-level exposure + SOC
# -----------------------------
qc_2x2 <- dbGetQuery(con, sprintf("
  WITH case_level AS (
    SELECT
      caseid,
      MAX(
        CASE
          WHEN role_cod IN ('PS','SS')
           AND drugname IN (SELECT drugname FROM drug_list)
          THEN 1 ELSE 0
        END
      ) AS exposed,
      MAX(
        CASE
          WHEN soc_name = '%s'
          THEN 1 ELSE 0
        END
      ) AS has_soc
    FROM faers
    GROUP BY caseid
  )
  SELECT
    SUM(CASE WHEN exposed=1 AND has_soc=1 THEN 1 ELSE 0 END) AS a_exposed_soc,
    SUM(CASE WHEN exposed=1 AND has_soc=0 THEN 1 ELSE 0 END) AS b_exposed_no_soc,
    SUM(CASE WHEN exposed=0 AND has_soc=1 THEN 1 ELSE 0 END) AS c_unexposed_soc,
    SUM(CASE WHEN exposed=0 AND has_soc=0 THEN 1 ELSE 0 END) AS d_unexposed_no_soc
  FROM case_level
", soc_target))

print(qc_2x2)
