## =========================================================
## Figure 6B — Molecule-level Risk Portrait (Data Only)
## Inherited from drug-level pipeline
## Engine: DuckDB + Parquet
## =========================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(here)
  library(readr)
})

## ---------------------------------------------------------
## 0. DuckDB connection (same style as drug-level script)
## ---------------------------------------------------------
duckdb_dir <- here("duckdb")
if (!dir.exists(duckdb_dir)) {
  dir.create(duckdb_dir, recursive = TRUE)
}

duckdb_file <- file.path(duckdb_dir, "FAERS_Figure6B_portrait.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file)


dbExecute(con, "PRAGMA threads=8;")
dbExecute(con, "PRAGMA memory_limit='20GB';")

on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

## ---------------------------------------------------------
## 1. MASTER parquet (locked)
## ---------------------------------------------------------
faers_parquet <- "D:/FAERS/MASTER/FAERS_MASTER_FILE_2004-2024_with_serious.parquet"

soc_target <- "Hepatobiliary disorders"

## ---------------------------------------------------------
## 2. Case-level DILI flag (SOC-based)
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
## 3. Case-level demographics
## ---------------------------------------------------------
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE case_demo AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    MAX(sex_std)   AS sex_std,
    MAX(age_group) AS age_group
  FROM read_parquet('%s')
  WHERE caseid IS NOT NULL
  GROUP BY caseid
", faers_parquet))

## ---------------------------------------------------------
## 4. PS / SS drug records
## ---------------------------------------------------------
dbExecute(con, sprintf("
  CREATE OR REPLACE TEMP TABLE drug_psss AS
  SELECT
    CAST(caseid AS VARCHAR) AS caseid,
    UPPER(TRIM(drugname))   AS drugname
  FROM read_parquet('%s')
  WHERE role_cod IN ('PS','SS')
    AND caseid IS NOT NULL
    AND drugname IS NOT NULL
", faers_parquet))

## ---------------------------------------------------------
## 5. Molecule exposure flags (Figure 6B locked molecules)
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE molecule_flags AS
  SELECT
    caseid,
    MAX(CASE WHEN drugname IN
      ('TACROLIMUS','FK506','PROGRAF','ADVAGRAF')
      THEN 1 ELSE 0 END) AS flag_tacrolimus,

    MAX(CASE WHEN drugname IN
      ('BASILIXIMAB','SIMULECT')
      THEN 1 ELSE 0 END) AS flag_basiliximab,

    MAX(CASE WHEN drugname IN
      ('AZATHIOPRINE','IMURAN')
      THEN 1 ELSE 0 END) AS flag_azathioprine,

    MAX(CASE WHEN drugname IN
      ('EVEROLIMUS','AFINITOR','CERTICAN')
      THEN 1 ELSE 0 END) AS flag_everolimus
  FROM drug_psss
  GROUP BY caseid
")

## ---------------------------------------------------------
## 6. Assemble portrait base table (case-level)
## ---------------------------------------------------------
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE portrait_base AS
  SELECT
    d.caseid,
    d.DILI,
    demo.sex_std,
    demo.age_group,
    COALESCE(m.flag_tacrolimus,0)   AS flag_tacrolimus,
    COALESCE(m.flag_basiliximab,0)  AS flag_basiliximab,
    COALESCE(m.flag_azathioprine,0) AS flag_azathioprine,
    COALESCE(m.flag_everolimus,0)   AS flag_everolimus
  FROM case_dili d
  LEFT JOIN case_demo demo
    ON d.caseid = demo.caseid
  LEFT JOIN molecule_flags m
    ON d.caseid = m.caseid
")

## ---------------------------------------------------------
## 7. Pull to R (for downstream modeling / OR_combo)
## ---------------------------------------------------------
portrait_df <- dbGetQuery(con, "
  SELECT *
  FROM portrait_base
")

## ---------------------------------------------------------
## 8. Basic QC summary (optional but recommended)
## ---------------------------------------------------------
qc_summary <- portrait_df %>%
  summarise(
    n_cases = n(),
    n_dili  = sum(DILI, na.rm = TRUE),
    dili_rate = round(mean(DILI, na.rm = TRUE), 5)
  )

print(qc_summary)

## ---------------------------------------------------------
## 9. Save derived table for Figure 6B downstream use
## ---------------------------------------------------------
out_dir <- here("06_molecule_heterogeneity", "derived")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  portrait_df,
  file.path(out_dir, "Figure6B_portrait_base_table.csv")
)

cat("\nFigure 6B portrait base table saved to:\n", out_dir, "\n")

library(tidyverse)
library(here)

portrait <- read_csv(
  here("06_molecule_heterogeneity", "derived",
       "Figure6B_portrait_base_table.csv")
)
beta <- c(
  tacrolimus   = log(2.234203504),
  basiliximab = log(1.15175912),
  azathioprine= log(1.622455824),
  everolimus  = log(1.26821824),
  
  sex_male    = log(1.254780114),
  sex_other   = log(1.079740415),
  
  age_40_64   = log(0.938028595),
  age_65p     = log(1.052191783)
)
portrait_risk <- portrait %>%
  mutate(
    beta_molecule = case_when(
      flag_tacrolimus == 1   ~ beta["tacrolimus"],
      flag_basiliximab == 1 ~ beta["basiliximab"],
      flag_azathioprine == 1~ beta["azathioprine"],
      flag_everolimus == 1  ~ beta["everolimus"],
      TRUE ~ 0
    ),
    
    beta_sex = case_when(
      sex_std == "Male"  ~ beta["sex_male"],
      sex_std == "Other" ~ beta["sex_other"],
      TRUE ~ 0
    ),
    
    beta_age = case_when(
      age_group == "40-64" ~ beta["age_40_64"],
      age_group == "65+"   ~ beta["age_65p"],
      TRUE ~ 0
    ),
    
    logOR_combo = beta_molecule + beta_sex + beta_age,
    OR_combo    = exp(logOR_combo)
  )
risk_portrait_table <- portrait_risk %>%
  filter(
    flag_tacrolimus + flag_basiliximab +
      flag_azathioprine + flag_everolimus == 1
  ) %>%
  group_by(
    molecule = case_when(
      flag_tacrolimus == 1   ~ "Tacrolimus",
      flag_basiliximab == 1 ~ "Basiliximab",
      flag_azathioprine == 1~ "Azathioprine",
      flag_everolimus == 1  ~ "Everolimus"
    ),
    sex_std,
    age_group
  ) %>%
  summarise(
    OR_combo = mean(OR_combo),
    .groups = "drop"
  )
library(ggplot2)

risk_portrait_table$age_group <- factor(
  risk_portrait_table$age_group,
  levels = c("<18", "18-39", "40-64", "65+")
)

risk_portrait_table$sex_std <- factor(
  risk_portrait_table$sex_std,
  levels = c("Female", "Male", "Other")
)

p6b <- ggplot(
  risk_portrait_table,
  aes(x = age_group, y = sex_std, fill = OR_combo)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = sprintf("%.2f", OR_combo)),
    size = 3, color = "black"
  ) +
  scale_fill_gradient2(
    low = "#F6F1EA",
    mid = "#C2C9C3",
    high = "#8E3232",
    midpoint = 1,
    name = "OR (combo)"
  ) +
  facet_wrap(~ molecule, nrow = 1) +
  theme_classic(base_size = 12) +
  labs(x = "Age group", y = "Sex")

p6b
out_dir <- here("figures", "Figure6")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(out_dir, "Figure6B_risk_portrait.pdf"),
  p6b, width = 10, height = 3.5
)

ggsave(
  file.path(out_dir, "Figure6B_risk_portrait.png"),
  p6b, width = 10, height = 3.5, dpi = 300
)

