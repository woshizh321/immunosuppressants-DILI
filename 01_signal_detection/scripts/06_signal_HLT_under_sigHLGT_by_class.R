suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
})

# -----------------------------
# Inputs
# -----------------------------
faers_path  <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target  <- "Hepatobiliary disorders"

# HLGT signal table you already generated
hlgt_csv <- "02_hierarchical_signal_detection/outputs/HLGT/FAERS_HLGT_under_hepatobiliary_by_class_signal_table.csv"

# thresholds
min_cases_event_hlgt <- 20     # reuse your HLGT stability threshold
min_cases_event_hlt  <- 20     # HLT stability threshold
top_k_hlgt_per_class <- 3      # only expand top K HLGT per class (keep it small & interpretable)

# dict + metrics
source("99_shared/dict/immunosuppressants_drug_classes.R")
source("99_shared/functions/signal_metrics_basic.R")  # compute_signal_metrics(a,b,c,d)

# output
out_dir <- "02_hierarchical_signal_detection/outputs/HLT"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Read HLGT results and pick "significant HLGT" per class
# -----------------------------
if (!file.exists(hlgt_csv)) stop("HLGT result csv not found: ", hlgt_csv)

hlgt_tbl <- readr::read_csv(hlgt_csv, show_col_types = FALSE)

# robust column checks (QC-first)
need_cols <- c("hlgt_name", "drug_class", "n_event_total", "ROR")
miss <- setdiff(need_cols, names(hlgt_tbl))
if (length(miss) > 0) stop("Missing columns in HLGT table: ", paste(miss, collapse = ", "))

sig_hlgt <- hlgt_tbl %>%
  filter(!is.na(hlgt_name), !is.na(drug_class)) %>%
  filter(n_event_total >= min_cases_event_hlgt) %>%
  group_by(drug_class) %>%
  arrange(desc(ROR), .by_group = TRUE) %>%
  slice_head(n = top_k_hlgt_per_class) %>%
  ungroup() %>%
  distinct(drug_class, hlgt_name)

cat("Selected HLGT for expansion (per class):\n")
print(sig_hlgt)

if (nrow(sig_hlgt) == 0) stop("No HLGT selected for expansion. Check thresholds / HLGT table.")

# -----------------------------
# DuckDB: file-backed (spill-safe)
# -----------------------------
duckdb_dir  <- "99_shared/tmp_duckdb"
dir.create(duckdb_dir, recursive = TRUE, showWarnings = FALSE)
duckdb_file <- file.path(duckdb_dir, "hlt_work.duckdb")

con <- dbConnect(duckdb(), dbdir = duckdb_file)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")
dbExecute(con, sprintf("PRAGMA temp_directory='%s';",
                       normalizePath(duckdb_dir, winslash = "/", mustWork = FALSE)))
dbExecute(con, "PRAGMA enable_object_cache=true;")
dbExecute(con, "SET preserve_insertion_order=false;")

# -----------------------------
# Materialize slim tables (one-time)
# -----------------------------
all_is_drugs <- unique(toupper(unlist(drug_classes)))
dbWriteTable(con, "all_is_drug_list",
             data.frame(drugname = all_is_drugs, stringsAsFactors = FALSE),
             overwrite = TRUE)

# total distinct cases
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE total_cases AS
  SELECT COUNT(DISTINCT caseid)::BIGINT AS n_total
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL;
", faers_path))
n_total <- dbGetQuery(con, "SELECT n_total FROM total_cases;")$n_total[1]
cat("Total distinct cases:", n_total, "\n")

# hepatobiliary: case × (HLGT, HLT) distinct table (this is the key event universe)
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE hep_case_hlgt_hlt AS
  SELECT DISTINCT
    CAST(caseid AS VARCHAR) AS caseid,
    hlgt_name,
    hlt_name
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND soc_name = '%s'
    AND hlgt_name IS NOT NULL
    AND hlt_name IS NOT NULL;
", faers_path, soc_target))

# PS/SS + only immunosuppressant drug rows (huge reduction)
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

# -----------------------------
# Loop: class -> selected HLGTs -> HLT signals
# -----------------------------
results <- list()

for (cls in names(drug_classes)) {
  
  cat("\n=== Processing HLT under selected HLGTs for:", cls, "===\n")
  
  # selected HLGTs for this class
  hlgt_vec <- sig_hlgt %>% filter(drug_class == cls) %>% pull(hlgt_name)
  if (length(hlgt_vec) == 0) {
    cat("No HLGT selected for this class. Skip.\n")
    next
  }
  
  # exposure cases for this class (from prefiltered psss_is_rows)
  drug_vec <- unique(toupper(drug_classes[[cls]]))
  dbWriteTable(con, "drug_list",
               data.frame(drugname = drug_vec, stringsAsFactors = FALSE),
               overwrite = TRUE)
  
  dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE expo_case AS
    SELECT caseid, 1 AS exposed
    FROM psss_is_rows
    WHERE drugname IN (SELECT drugname FROM drug_list)
    GROUP BY caseid;
  ")
  
  n_exposed <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM expo_case;")$n[1]
  n_unexposed <- n_total - n_exposed
  cat("n_exposed:", n_exposed, " n_unexposed:", n_unexposed, "\n")
  
  # loop HLGTs (small K)
  for (hlgt_nm in hlgt_vec) {
    
    cat("  -> HLGT:", hlgt_nm, "\n")
    
    # Pull HLT event counts (a/c) under this HLGT, by exposed (0/1)
    # Note: use safe SQL string escaping for single quotes
    hlgt_sql <- str_replace_all(hlgt_nm, "'", "''")
    
    agg <- dbGetQuery(con, sprintf("
      WITH evt AS (
        SELECT
          h.hlt_name,
          COALESCE(e.exposed, 0) AS exposed,
          COUNT(DISTINCT h.caseid) AS n_evt_cases
        FROM hep_case_hlgt_hlt h
        LEFT JOIN expo_case e
          ON h.caseid = e.caseid
        WHERE h.hlgt_name = '%s'
        GROUP BY h.hlt_name, exposed
      )
      SELECT
        hlt_name,
        SUM(CASE WHEN exposed=1 THEN n_evt_cases ELSE 0 END)::BIGINT AS a,
        SUM(CASE WHEN exposed=0 THEN n_evt_cases ELSE 0 END)::BIGINT AS c
      FROM evt
      GROUP BY hlt_name;
    ", hlgt_sql))
    
    if (nrow(agg) == 0) next
    
    met <- agg %>%
      mutate(
        hlgt_name = hlgt_nm,
        n_exposed_cases   = n_exposed,
        n_unexposed_cases = n_unexposed,
        b = n_exposed_cases - a,
        d = n_unexposed_cases - c,
        n_event_total = a + c
      ) %>%
      filter(n_event_total >= min_cases_event_hlt) %>%
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
        level = "HLT",
        soc = soc_target
      ) %>%
      arrange(desc(ROR))
    
    if (nrow(met) > 0) {
      results[[paste(cls, hlgt_nm, sep = " | ")]] <- met
    }
  }
}

hlt_results <- bind_rows(results)

# -----------------------------
# Save
# -----------------------------
out_csv <- file.path(out_dir, "FAERS_HLT_under_sigHLGT_under_hepatobiliary_by_class_signal_table.csv")
write_csv(hlt_results, out_csv)

cat("\nSaved:", out_csv, "\n")
cat("Rows:", nrow(hlt_results), "\n")

# Quick peek: top 5 HLT per class (overall)
print(
  hlt_results %>%
    group_by(drug_class) %>%
    slice_head(n = 5) %>%
    ungroup()
)

