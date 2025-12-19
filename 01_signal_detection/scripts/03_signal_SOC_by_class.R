suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
})

# -----------------------------
# Inputs
# -----------------------------
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target <- "Hepatobiliary disorders"

# drug classes (locked)
source("99_shared/dict/immunosuppressants_drug_classes.R")

# signal metric
source("99_shared/functions/signal_metrics_basic.R")

# -----------------------------
# Connect duckDB
# -----------------------------
con <- dbConnect(duckdb(), dbdir=":memory:")
on.exit(dbDisconnect(con, shutdown=TRUE), add=TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")

# Base FAERS view
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

results <- list()

# -----------------------------
# Loop over drug classes
# -----------------------------
for (cls in names(drug_classes)) {
  
  cat("Processing:", cls, "\n")
  
  drug_vec <- toupper(drug_classes[[cls]])
  
  # register drug list
  dbWriteTable(
    con,
    "drug_list",
    data.frame(drugname = drug_vec, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  
  tab <- dbGetQuery(con, sprintf("
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
        ) AS event
      FROM faers
      GROUP BY caseid
    )
    SELECT
      SUM(CASE WHEN exposed=1 AND event=1 THEN 1 ELSE 0 END) AS a,
      SUM(CASE WHEN exposed=1 AND event=0 THEN 1 ELSE 0 END) AS b,
      SUM(CASE WHEN exposed=0 AND event=1 THEN 1 ELSE 0 END) AS c,
      SUM(CASE WHEN exposed=0 AND event=0 THEN 1 ELSE 0 END) AS d
    FROM case_level
  ", soc_target))
  
  res <- compute_signal_metrics(tab$a, tab$b, tab$c, tab$d)
  res$drug_class <- cls
  res$level <- "SOC"
  res$term  <- soc_target
  
  results[[cls]] <- res
}

soc_results <- bind_rows(results)

print(soc_results)
