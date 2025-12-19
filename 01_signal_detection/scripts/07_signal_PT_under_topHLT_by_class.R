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
faers_path <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
soc_target <- "Hepatobiliary disorders"

# HLT signal table you already generated (Rows: 83)
hlt_csv <- "02_hierarchical_signal_detection/outputs/HLT/FAERS_HLT_under_sigHLGT_under_hepatobiliary_by_class_signal_table.csv"

# selection + thresholds
top_k_hlt_per_class <- 2
min_cases_event_hlt_for_select <- 20   # use stable HLT to choose top2
min_cases_event_pt <- 10               # PT stability threshold for Supplementary

# dict + metrics
source("99_shared/dict/immunosuppressants_drug_classes.R")
source("99_shared/functions/signal_metrics_basic.R")  # compute_signal_metrics(a,b,c,d)

# outputs
out_dir <- "02_hierarchical_signal_detection/outputs/PT"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) Select Top-2 HLT per class from existing HLT table
# -----------------------------
if (!file.exists(hlt_csv)) stop("HLT result csv not found: ", hlt_csv)

hlt_tbl <- readr::read_csv(hlt_csv, show_col_types = FALSE)

need_cols <- c("drug_class","hlt_name","n_event_total","ROR")
miss <- setdiff(need_cols, names(hlt_tbl))
if (length(miss) > 0) stop("Missing columns in HLT table: ", paste(miss, collapse=", "))

top_hlt <- hlt_tbl %>%
  filter(!is.na(drug_class), !is.na(hlt_name)) %>%
  filter(n_event_total >= min_cases_event_hlt_for_select) %>%
  group_by(drug_class) %>%
  arrange(desc(ROR), .by_group = TRUE) %>%
  slice_head(n = top_k_hlt_per_class) %>%
  ungroup() %>%
  distinct(drug_class, hlt_name)

cat("Selected Top HLT per class for PT validation:\n")
print(top_hlt)

if (nrow(top_hlt) == 0) stop("No HLT selected. Check thresholds / HLT table.")

# Save selection for traceability
readr::write_csv(top_hlt, file.path(out_dir, "PT_validation_selected_topHLT_per_class.csv"))

# -----------------------------
# 2) DuckDB (file-backed, spill-safe)
# -----------------------------
duckdb_dir  <- "99_shared/tmp_duckdb"
dir.create(duckdb_dir, recursive = TRUE, showWarnings = FALSE)
duckdb_file <- file.path(duckdb_dir, "pt_under_hlt_work.duckdb")

con <- dbConnect(duckdb(), dbdir = duckdb_file)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='22GB';")
dbExecute(con, sprintf("PRAGMA temp_directory='%s';",
                       normalizePath(duckdb_dir, winslash = "/", mustWork = FALSE)))
dbExecute(con, "PRAGMA enable_object_cache=true;")
dbExecute(con, "SET preserve_insertion_order=false;")

# -----------------------------
# 3) Materialize slim tables (one-time)
# -----------------------------
all_is_drugs <- unique(toupper(unlist(drug_classes)))
dbWriteTable(con, "all_is_drug_list",
             data.frame(drugname = all_is_drugs, stringsAsFactors = FALSE),
             overwrite = TRUE)

# total cases
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE total_cases AS
  SELECT COUNT(DISTINCT caseid)::BIGINT AS n_total
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL;
", faers_path))
n_total <- dbGetQuery(con, "SELECT n_total FROM total_cases;")$n_total[1]
cat("Total distinct cases:", n_total, "\n")

# hepatobiliary: case × HLT × PT (event universe)
# NOTE: use pt_name (you already have it in master)
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE hep_case_hlt_pt AS
  SELECT DISTINCT
    CAST(caseid AS VARCHAR) AS caseid,
    hlt_name,
    pt_name
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
    AND soc_name = '%s'
    AND hlt_name IS NOT NULL
    AND pt_name IS NOT NULL;
", faers_path, soc_target))

# PS/SS + immunosuppressant drug rows only
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
# 4) Loop: drug_class -> selected HLTs -> PT signals within that HLT
# -----------------------------
results <- list()

for (cls in names(drug_classes)) {
  
  cat("\n=== PT validation for class:", cls, "===\n")
  
  # selected HLTs for this class
  hlt_vec <- top_hlt %>% filter(drug_class == cls) %>% pull(hlt_name)
  if (length(hlt_vec) == 0) {
    cat("No selected HLT for this class (maybe filtered out). Skip.\n")
    next
  }
  
  # exposure cases for this class
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
  
  # Loop each selected HLT (Top2)
  for (hlt_nm in hlt_vec) {
    
    cat("  -> HLT:", hlt_nm, "\n")
    hlt_sql <- str_replace_all(hlt_nm, "'", "''")
    
    agg <- dbGetQuery(con, sprintf("
      WITH evt AS (
        SELECT
          h.pt_name,
          COALESCE(e.exposed, 0) AS exposed,
          COUNT(DISTINCT h.caseid) AS n_evt_cases
        FROM hep_case_hlt_pt h
        LEFT JOIN expo_case e
          ON h.caseid = e.caseid
        WHERE h.hlt_name = '%s'
        GROUP BY h.pt_name, exposed
      )
      SELECT
        pt_name,
        SUM(CASE WHEN exposed=1 THEN n_evt_cases ELSE 0 END)::BIGINT AS a,
        SUM(CASE WHEN exposed=0 THEN n_evt_cases ELSE 0 END)::BIGINT AS c
      FROM evt
      GROUP BY pt_name;
    ", hlt_sql))
    
    if (nrow(agg) == 0) next
    
    met <- agg %>%
      mutate(
        hlt_name = hlt_nm,
        n_exposed_cases   = n_exposed,
        n_unexposed_cases = n_unexposed,
        b = n_exposed_cases - a,
        d = n_unexposed_cases - c,
        n_event_total = a + c
      ) %>%
      filter(n_event_total >= min_cases_event_pt) %>%
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
        level = "PT_under_topHLT",
        soc = soc_target
      ) %>%
      arrange(desc(ROR))
    
    if (nrow(met) > 0) {
      results[[paste(cls, hlt_nm, sep=" | ")]] <- met
    }
  }
}

pt_results <- bind_rows(results)

# -----------------------------
# 5) Save outputs
# -----------------------------
out_csv <- file.path(out_dir, "FAERS_PT_under_topHLT_under_hepatobiliary_by_class_signal_table.csv")
readr::write_csv(pt_results, out_csv)

cat("\nSaved:", out_csv, "\n")
cat("Rows:", nrow(pt_results), "\n")

# quick peek: top 5 PT per class (overall)
print(
  pt_results %>%
    group_by(drug_class) %>%
    slice_head(n = 5) %>%
    ungroup()
)

