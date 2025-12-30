# =========================================================
# Table 1 (DILI): MASTER summary by immunosuppressant class
#   - Annual DILI counts (2004–2024)
#   - Sex distribution (Male/Female/Other)
#   - Age distribution (median [IQR])
#   - Region distribution
# Tools: duckdb + arrow, QC-first, PS+SS exposure only
# =========================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(glue)
  library(data.table)
  library(stringr)
  library(arrow)
})

# -------------------------
# 0) User config
# -------------------------
MASTER_FILE <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"
DICT_R      <- "D:/Projects/immunosuppressants_DILI/99_shared/dict/immunosuppressants_drug_classes.R"
OUT_DIR     <- "D:/Projects/immunosuppressants_DILI/01_table1_outputs"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------------
# 1) Load dictionary (must define `drug_classes` list)
# -------------------------
source(DICT_R)
stopifnot(exists("drug_classes"), is.list(drug_classes), length(drug_classes) > 0)

# Normalize dict to uppercase for robust matching
drug_classes_u <- lapply(drug_classes, function(x) unique(toupper(trimws(x))))
drug_class_names <- names(drug_classes_u)

message("[OK] Loaded drug_classes: ", paste(drug_class_names, collapse = " | "))

# -------------------------
# 2) DuckDB connection
# -------------------------
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = TRUE)
dbExecute(con, "PRAGMA threads=4;")
dbExecute(con, "PRAGMA memory_limit='8GB';")

norm_path <- function(p) gsub("\\\\", "/", p)

# -------------------------
# 3) QC-first: inspect schema, pick columns without guessing
# -------------------------
# Read 0 rows to get schema quickly (via duckdb)
cols0 <- dbGetQuery(con, glue("SELECT * FROM read_parquet('{norm_path(MASTER_FILE)}') LIMIT 0"))
cols  <- names(cols0)

message("\n[QC] N columns = ", length(cols))
message("[QC] Sample columns: ", paste(head(cols, 25), collapse = ", "), if (length(cols) > 25) " ...")

pick_col <- function(cols, candidates) {
  ci <- tolower(cols)
  for (cand in candidates) {
    hit <- which(ci == tolower(cand))
    if (length(hit) > 0) return(cols[hit[1]])
  }
  return(NA_character_)
}

# Core identifiers / drug / role
col_id    <- pick_col(cols, c("primaryid","PRIMARYID","case_key","CASE_KEY","id","ID"))
col_drug  <- pick_col(cols, c("drugname","DRUGNAME","drugname_en","DRUGNAME_EN","prod_ai","PROD_AI"))
col_role  <- pick_col(cols, c("role_cod","ROLE_COD","role_code","ROLE_CODE","role","ROLE","role_std","ROLE_STD"))

# Event date / year
col_year  <- pick_col(cols, c("year","YEAR","fda_year","FDA_YEAR"))
col_evt   <- pick_col(cols, c("event_dt","EVENT_DT","event_dt_num","EVENT_DT_NUM","fda_dt","FDA_DT","fda_dt_num","FDA_DT_NUM"))

# Demographics
col_sex   <- pick_col(cols, c("sex","SEX","sex_std","SEX_STD"))
col_age   <- pick_col(cols, c("age","AGE","age_years","AGE_YEARS","age_num","AGE_NUM"))
col_agecd <- pick_col(cols, c("age_cod","AGE_COD","age_unit","AGE_UNIT"))

# Geography
col_country1 <- pick_col(cols, c("occr_country","OCCR_COUNTRY","reporter_country","REPORTER_COUNTRY","country","COUNTRY","country_use","COUNTRY_USE"))
col_region   <- pick_col(cols, c("region","REGION","region_std","REGION_STD","continent","CONTINENT"))

# MedDRA SOC for DILI
col_socname <- pick_col(cols, c("soc_name","SOC_NAME","soc","SOC","soc_term","SOC_TERM"))
col_soccode <- pick_col(cols, c("soc_code","SOC_CODE"))

# QC report
qc <- data.table(
  field = c("ID","DRUGNAME","ROLE","YEAR","EVENT_DT","SEX","AGE","AGE_COD","COUNTRY","REGION","SOC_NAME","SOC_CODE"),
  chosen_column = c(col_id,col_drug,col_role,col_year,col_evt,col_sex,col_age,col_agecd,col_country1,col_region,col_socname,col_soccode)
)
fwrite(qc, file.path(OUT_DIR, "QC_column_mapping.csv"))
print(qc)

# Hard requirements
if (is.na(col_id))   stop("Missing case identifier column (e.g., primaryid). Check QC_column_mapping.csv")
if (is.na(col_drug)) stop("Missing drug name column. Check QC_column_mapping.csv")
if (is.na(col_role)) stop("Missing role column (need PS/SS). Check QC_column_mapping.csv")

# Need SOC_NAME for DILI definition (preferred); otherwise stop (to avoid guessing SOC code mapping)
if (is.na(col_socname)) {
  stop("Missing SOC_NAME column. For DILI outcome as Hepatobiliary disorders, SOC_NAME is required to avoid guessing. Please confirm your SOC column name.")
}

# -------------------------
# 4) Build base view (minimal columns)
# -------------------------
sql_or_null <- function(col, alias) if (is.na(col)) glue("NULL AS {alias}") else glue("{col} AS {alias}")

# Derive YEAR if not present
year_expr <- if (!is.na(col_year)) {
  glue("TRY_CAST({col_year} AS INTEGER)")
} else if (!is.na(col_evt)) {
  # handle YYYYMMDD numeric or string
  glue("
    CASE
      WHEN regexp_matches(CAST({col_evt} AS VARCHAR), '^[0-9]{{8}}$')
        THEN TRY_CAST(SUBSTR(CAST({col_evt} AS VARCHAR), 1, 4) AS INTEGER)
      WHEN regexp_matches(CAST({col_evt} AS VARCHAR), '^[0-9]{{4}}$')
        THEN TRY_CAST(CAST({col_evt} AS VARCHAR) AS INTEGER)
      ELSE NULL
    END
  ")
} else {
  "NULL"
}

# Age-to-years expression (robust)
age_years_expr <- if (!is.na(col_age) && !is.na(col_agecd)) {
  glue("
    CASE
      WHEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE) IS NULL THEN NULL
      WHEN UPPER(COALESCE({col_agecd},'')) IN ('','YR','YRS','YEAR','YEARS') THEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)
      WHEN UPPER({col_agecd}) IN ('MON','MONTH','MONTHS') THEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)/12.0
      WHEN UPPER({col_agecd}) IN ('WK','WEEK','WEEKS') THEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)/52.0
      WHEN UPPER({col_agecd}) IN ('DY','DAY','DAYS') THEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)/365.25
      WHEN UPPER({col_agecd}) IN ('DEC','DECADE') THEN TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)*10.0
      ELSE NULL
    END
  ")
} else if (!is.na(col_age)) {
  glue("TRY_CAST(NULLIF(CAST({col_age} AS VARCHAR), '') AS DOUBLE)")
} else {
  "NULL"
}

# Standardize sex to Male/Female/Other
sex3_expr <- if (!is.na(col_sex)) {
  glue("
    CASE
      WHEN UPPER(COALESCE({col_sex},'')) IN ('M','MALE','1') THEN 'Male'
      WHEN UPPER(COALESCE({col_sex},'')) IN ('F','FEMALE','2') THEN 'Female'
      ELSE 'Other'
    END
  ")
} else {
  "'Other'"
}

# Country_use
country_expr <- if (!is.na(col_country1)) {
  glue("UPPER(NULLIF(CAST({col_country1} AS VARCHAR), ''))")
} else {
  "NULL"
}

# Region: if region column exists, use it; else will map from country later in R
region_expr <- if (!is.na(col_region)) {
  glue("NULLIF(CAST({col_region} AS VARCHAR), '')")
} else {
  "NULL"
}

dbExecute(con, glue("
  CREATE OR REPLACE VIEW v_base AS
  SELECT
    {col_id}    AS case_key,
    {year_expr} AS year,
    {sex3_expr} AS sex3,
    {age_years_expr} AS age_years,
    {country_expr} AS country,
    {region_expr}  AS region,
    UPPER(COALESCE({col_role},'')) AS role_cod,
    UPPER(COALESCE({col_drug},'')) AS drugname_u,
    UPPER(COALESCE({col_socname},'')) AS soc_name_u
  FROM read_parquet('{norm_path(MASTER_FILE)}')
"))

# Quick QC
qc_counts <- dbGetQuery(con, "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT case_key) AS n_cases FROM v_base")
print(qc_counts)

# -------------------------
# 5) Define PS+SS exposure flags by class (case-level)  [FIXED]
#    - Avoid '.' in DuckDB table names ('.' is schema separator)
# -------------------------

# make a safe id for SQL objects: [a-z0-9_], no dots
safe_id <- function(x){
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)   # non-alnum -> _
  x <- gsub("^_+|_+$", "", x)       # trim _
  x
}

class_id <- setNames(vapply(drug_class_names, safe_id, character(1)), drug_class_names)
print(data.table(drug_class = drug_class_names, class_id = unname(class_id)))

# Build LIKE OR condition safely
like_or <- function(vec) {
  vec <- vec[!is.na(vec) & nzchar(vec)]
  if (length(vec) == 0) return("FALSE")
  # escape single quote (rare in drug names, but safe)
  vec2 <- gsub("'", "''", vec)
  paste0("(", paste(sprintf("drugname_u LIKE '%%%s%%'", vec2), collapse = " OR "), ")")
}

# Create class-specific case flag tables
for (cls in drug_class_names) {
  dict_vec <- drug_classes_u[[cls]]
  cond <- like_or(dict_vec)
  
  tbl <- paste0("flag_", class_id[[cls]])  # e.g., flag_calcineurin_inhibitors
  
  dbExecute(con, glue("
    CREATE OR REPLACE TABLE {tbl} AS
    SELECT DISTINCT case_key, 1 AS flag
    FROM v_base
    WHERE role_cod IN ('PS','SS') AND {cond}
  "))
  
  nflag <- dbGetQuery(con, glue("SELECT COUNT(*) AS n_cases FROM {tbl}"))
  message("[QC] ", cls, " (", tbl, ") flagged cases = ", nflag$n_cases)
}

# Combine flags into one case-level table
flag_selects <- paste0(
  sapply(drug_class_names, function(cls) {
    tbl <- paste0("flag_", class_id[[cls]])
    alias <- paste0("flag_", class_id[[cls]])
    glue("MAX(COALESCE({tbl}.flag,0)) AS {alias}")
  }),
  collapse = ",\n"
)

flag_joins <- paste0(
  sapply(drug_class_names, function(cls) {
    tbl <- paste0("flag_", class_id[[cls]])
    glue("LEFT JOIN {tbl} USING(case_key)")
  }),
  collapse = "\n"
)

# Rebuild t_case_flags correctly: JOIN first, then aggregate
flag_join_sql <- paste0(
  sapply(drug_class_names, function(cls) {
    tbl <- paste0("flag_", class_id[[cls]])
    # join each flag table to base rows by case_key
    glue("LEFT JOIN {tbl} ON b.case_key = {tbl}.case_key")
  }),
  collapse = "\n"
)

dbExecute(con, glue("
  CREATE OR REPLACE TABLE t_case_flags AS
  SELECT
    b.case_key,
    MIN(b.year) AS year,
    -- sex/age/country/region：取该 case 内任意一个非空值即可（Table1 不做病例内一致性推断）
    MAX(b.sex3) AS sex3,
    MAX(b.age_years) AS age_years,
    MAX(b.country) AS country,
    MAX(b.region)  AS region,
    {flag_selects}
  FROM v_base b
  {flag_join_sql}
  GROUP BY b.case_key
"))


# For later: list of flag columns and a sum expression
flag_cols <- paste0("flag_", unname(class_id))   # e.g. flag_calcineurin_inhibitors ...
flag_sum_expr <- paste(flag_cols, collapse = " + ")

message("[QC] flag_cols = ", paste(flag_cols, collapse = ", "))
message("[QC] flag_sum_expr = ", flag_sum_expr)

# -------------------------
# 6) Define DILI outcome (case-level): SOC contains "HEPATOBILIARY"
# -------------------------
# DILI if any row in case has SOC_NAME ~ hepatobiliary
dbExecute(con, "
  CREATE OR REPLACE TABLE t_case_dili AS
  SELECT DISTINCT case_key, 1 AS dili
  FROM v_base
  WHERE soc_name_u LIKE '%HEPATOBILIARY%'
")

# Merge flags + outcome; keep only cases exposed to ANY of 5 classes (optional)
flag_cols <- paste0("flag_", make.names(drug_class_names))
flag_sum_expr <- paste(flag_cols, collapse = " + ")

dbExecute(con, glue("
  CREATE OR REPLACE TABLE t_case_final AS
  SELECT
    f.*,
    COALESCE(d.dili,0) AS dili
  FROM t_case_flags f
  LEFT JOIN t_case_dili d USING(case_key)
"))

# Optional: restrict to immunosuppressant-exposed cases (any class flag=1)
dbExecute(con, glue("
  CREATE OR REPLACE TABLE t_case_final_exposed AS
  SELECT *
  FROM t_case_final
  WHERE ({flag_sum_expr}) > 0
"))

# -------------------------
# 7) Table 1 outputs  [FIXED flags]
# -------------------------

# Long-format: one row per (case, drug_class) where class flag = 1
long_union <- paste0(
  sapply(drug_class_names, function(cls) {
    fcol <- paste0("flag_", class_id[[cls]])
    glue("
      SELECT case_key, year, sex3, age_years, country, region, dili, '{cls}' AS drug_class
      FROM t_case_final_exposed
      WHERE {fcol} = 1
    ")
  }),
  collapse = "\nUNION ALL\n"
)

dbExecute(con, glue("CREATE OR REPLACE VIEW v_long AS {long_union}"))


# ---- 7.1 Annual DILI case counts by class (2004–2024) ----
annual <- dbGetQuery(con, "
  SELECT
    year,
    drug_class,
    COUNT(DISTINCT case_key) AS dili_cases
  FROM v_long
  WHERE dili = 1 AND year BETWEEN 2004 AND 2024
  GROUP BY year, drug_class
  ORDER BY drug_class, year
")
fwrite(as.data.table(annual), file.path(OUT_DIR, "Table1_Annual_DILI_by_Class.csv"))

# ---- 7.2 Sex distribution by class among DILI cases ----
sex_tab <- dbGetQuery(con, "
  SELECT
    drug_class,
    sex3,
    COUNT(DISTINCT case_key) AS n
  FROM v_long
  WHERE dili = 1
  GROUP BY drug_class, sex3
  ORDER BY drug_class, sex3
")
sex_dt <- as.data.table(sex_tab)
sex_dt[, total := sum(n), by = drug_class]
sex_dt[, pct := round(100 * n / total, 2)]
fwrite(sex_dt, file.path(OUT_DIR, "Table1_Sex_by_Class.csv"))

# ---- 7.3 Age distribution by class among DILI cases: median [IQR] ----
age_tab <- dbGetQuery(con, "
  SELECT
    drug_class,
    COUNT(age_years) AS n_nonmissing,
    quantile_cont(age_years, 0.5)  AS median,
    quantile_cont(age_years, 0.25) AS q1,
    quantile_cont(age_years, 0.75) AS q3
  FROM v_long
  WHERE dili = 1 AND age_years IS NOT NULL
  GROUP BY drug_class
  ORDER BY drug_class
")
fwrite(as.data.table(age_tab), file.path(OUT_DIR, "Table1_Age_by_Class.csv"))

# ---- 7.4 Region distribution among DILI cases ----
# If region column missing, map from country in R (continent as region)
final_cases <- as.data.table(dbGetQuery(con, "
  SELECT DISTINCT drug_class, case_key, year, sex3, age_years, country, region, dili
  FROM v_long
  WHERE dili = 1
"))

if (all(is.na(final_cases$region)) || all(final_cases$region == "")) {
  message("[INFO] REGION column missing/empty; deriving region from COUNTRY using `countrycode` (continent).")
  suppressPackageStartupMessages(library(countrycode))
  # country is typically ISO2; attempt conversion; unknown -> NA
  final_cases[, region := countrycode(country, origin = "iso2c", destination = "continent", warn = FALSE)]
}

reg_tab <- final_cases[!is.na(region) & region != "",
                       .(n = uniqueN(case_key)),
                       by = .(drug_class, region)
][order(drug_class, -n)]

reg_tab[, total := sum(n), by = drug_class]
reg_tab[, pct := round(100 * n / total, 2)]

fwrite(reg_tab, file.path(OUT_DIR, "Table1_Region_by_Class.csv"))

# ---- 7.5 Compact Table 1 (wide-ish) optional: Age+Sex summary row per class ----
age_fmt <- as.data.table(age_tab)
age_fmt[, age_median_iqr := sprintf("%.1f [%.1f–%.1f]", median, q1, q3)]
age_fmt <- age_fmt[, .(drug_class, age_median_iqr, n_age_nonmissing = n_nonmissing)]

sex_wide <- dcast(sex_dt, drug_class ~ sex3, value.var = c("n","pct"), fill = 0)
# create compact strings
if (all(c("n_Male","pct_Male","n_Female","pct_Female","n_Other","pct_Other") %in% names(sex_wide))) {
  sex_wide[, sex_Male   := sprintf("%d (%.2f%%)", n_Male, pct_Male)]
  sex_wide[, sex_Female := sprintf("%d (%.2f%%)", n_Female, pct_Female)]
  sex_wide[, sex_Other  := sprintf("%d (%.2f%%)", n_Other, pct_Other)]
  sex_wide <- sex_wide[, .(drug_class, sex_Male, sex_Female, sex_Other)]
}

reg_top3 <- reg_tab[, .SD[1:min(.N,3)], by = drug_class]
reg_top3[, region_top3 := paste0(region, ": ", n, " (", pct, "%)", collapse = "; "), by = drug_class]
reg_top3 <- unique(reg_top3[, .(drug_class, region_top3)])

table1_compact <- Reduce(function(x,y) merge(x,y, by="drug_class", all=TRUE),
                         list(age_fmt, sex_wide, reg_top3))
fwrite(table1_compact, file.path(OUT_DIR, "Table1_Compact_by_Class.csv"))

message("\n[DONE] Outputs written to: ", OUT_DIR)
dbDisconnect(con, shutdown = TRUE)

