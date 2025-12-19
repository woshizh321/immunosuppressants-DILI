suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
})

# -----------------------------
# Inputs
# -----------------------------
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target <- "Hepatobiliary disorders"
min_cases_event <- 20

source("99_shared/dict/immunosuppressants_drug_classes.R")
source("99_shared/functions/signal_metrics_basic.R")  # compute_signal_metrics(a,b,c,d)

out_dir <- "02_hierarchical_signal_detection/outputs/HLGT"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# ✅ Key optimization 1: use file-backed duckdb (spill to disk)
# -----------------------------
duckdb_dir <- "99_shared/tmp_duckdb"
dir.create(duckdb_dir, recursive = TRUE, showWarnings = FALSE)
duckdb_file <- file.path(duckdb_dir, "hlgt_work.duckdb")

con <- dbConnect(duckdb(), dbdir = duckdb_file)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")
dbExecute(con, sprintf("PRAGMA temp_directory='%s';",
                       normalizePath(duckdb_dir, winslash = "/", mustWork = FALSE)))

# small but helpful
dbExecute(con, "PRAGMA enable_object_cache=true;")
dbExecute(con, "SET preserve_insertion_order=false;")

# -----------------------------
# ✅ Key optimization 2: materialize once
#   (A) total case count
#   (B) hepatobiliary case × HLGT distinct table
#   (C) PS/SS drug rows restricted to ANY immunosuppressant (huge reduction)
# -----------------------------
all_is_drugs <- unique(toupper(unlist(drug_classes)))
dbWriteTable(con, "all_is_drug_list",
             data.frame(drugname = all_is_drugs, stringsAsFactors = FALSE),
             overwrite = TRUE)

# total cases (one-time)
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE total_cases AS
  SELECT COUNT(DISTINCT caseid)::BIGINT AS n_total
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL;
", faers_path))

# hepatobiliary case-hlgt (one-time, much smaller than full table)
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE hep_case_hlgt AS
  SELECT DISTINCT
    CAST(caseid AS VARCHAR) AS caseid,
    hlgt_name
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND soc_name = '%s'
    AND hlgt_name IS NOT NULL;
", faers_path, soc_target))

# PS/SS + only immunosuppressant drugs (one-time, huge reduction)
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE psss_is_rows AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname))   AS drugname
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND role_cod IN ('PS','SS')
    AND UPPER(TRIM(drugname)) IN (SELECT drugname FROM all_is_drug_list);
", faers_path))

n_total <- dbGetQuery(con, "SELECT n_total FROM total_cases;")$n_total[1]
cat("Total distinct cases:", n_total, "\n")

results <- list()

# -----------------------------
# Loop over drug classes (lightweight now)
# -----------------------------
for (cls in names(drug_classes)) {
  
  cat("=== Processing HLGT for:", cls, "===\n")
  
  drug_vec <- unique(toupper(drug_classes[[cls]]))
  dbWriteTable(con, "drug_list",
               data.frame(drugname = drug_vec, stringsAsFactors = FALSE),
               overwrite = TRUE)
  
  # case-level exposure for this class (from prefiltered psss_is_rows)
  dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE expo_case AS
    SELECT caseid, 1 AS exposed
    FROM psss_is_rows
    WHERE drugname IN (SELECT drugname FROM drug_list)
    GROUP BY caseid;
  ")
  
  n_exposed <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM expo_case;")$n[1]
  n_unexposed <- n_total - n_exposed
  
  # event counts per HLGT by exposed/unexposed
  # join hep_case_hlgt (event universe) with expo_case (exposure flag)
  agg <- dbGetQuery(con, sprintf("
    WITH evt AS (
      SELECT
        h.hlgt_name,
        COALESCE(e.exposed, 0) AS exposed,
        COUNT(DISTINCT h.caseid) AS n_evt_cases
      FROM hep_case_hlgt h
      LEFT JOIN expo_case e
        ON h.caseid = e.caseid
      GROUP BY h.hlgt_name, exposed
    )
    SELECT
      hlgt_name,
      SUM(CASE WHEN exposed=1 THEN n_evt_cases ELSE 0 END)::BIGINT AS a,
      SUM(CASE WHEN exposed=0 THEN n_evt_cases ELSE 0 END)::BIGINT AS c
    FROM evt
    GROUP BY hlgt_name;
  "))
  
  if (nrow(agg) == 0) next
  
  # build b/d + metrics in R (stable and simple)
  met <- agg %>%
    mutate(
      n_exposed_cases = n_exposed,
      n_unexposed_cases = n_unexposed,
      b = n_exposed_cases - a,
      d = n_unexposed_cases - c,
      n_event_total = a + c
    ) %>%
    filter(n_event_total >= min_cases_event) %>%
    rowwise() %>%
    mutate(
      tmp = list(compute_signal_metrics(a, b, c, d)),
      ROR   = tmp$ROR,
      ROR_L = tmp$ROR_L,
      ROR_U = tmp$ROR_U,
      PRR   = tmp$PRR,
      Chi2  = tmp$Chi2
    ) %>%
    ungroup() %>%
    select(-tmp) %>%
    mutate(
      drug_class = cls,
      level = "HLGT",
      soc = soc_target
    ) %>%
    arrange(desc(ROR))
  
  results[[cls]] <- met
}

hlgt_results <- bind_rows(results)

out_csv <- file.path(out_dir, "FAERS_HLGT_under_hepatobiliary_by_class_signal_table.csv")
write_csv(hlgt_results, out_csv)

cat("\nSaved:", out_csv, "\n")
cat("Rows:", nrow(hlgt_results), "\n")

# quick peek: top 5 per class
print(hlgt_results %>%
        group_by(drug_class) %>%
        slice_head(n = 5) %>%
        ungroup())
