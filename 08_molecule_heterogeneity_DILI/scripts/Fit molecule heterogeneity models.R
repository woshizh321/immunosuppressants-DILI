## =========================================================
## Fit molecule heterogeneity models (FIXED)
## - within-class uses ONE molecule factor (reference-based)
## - avoids mutually exclusive dummy collinearity
## - exports direct Tacrolimus vs Cyclosporine contrast for CNI
## =========================================================

log_msg("Step6: Fit molecule heterogeneity models (FIXED: molecule factor)")

suppressPackageStartupMessages({
  library(data.table)
  library(stats)
})

## ---------------------------
## 0) Load case-level data
## ---------------------------
case_level <- readRDS(file.path(DIRS$DATA, "case_level_molecule_table.rds"))
dt <- as.data.table(case_level)

## Ensure factor baselines consistent with training
dt[, sex_bin := factor(sex_bin, levels = SEX_BINS)]
dt[, age_bin := factor(age_bin, levels = AGE_BINS)]

## Identify regimen flags (class-level) and molecule flags
flag_cols <- grep("^flag_", names(dt), value = TRUE)
mol_cols  <- grep("^mol_",  names(dt), value = TRUE)

stopifnot("DILI" %in% names(dt))
stopifnot(length(flag_cols) > 0)
stopifnot(length(mol_cols)  > 0)

## ---------------------------
## Helpers
## ---------------------------

## Extract OR/CI for one coefficient term from glm summary
extract_or_ci <- function(fit, term) {
  co <- summary(fit)$coefficients
  if (!term %in% rownames(co)) return(NULL)
  beta <- unname(co[term, "Estimate"])
  se   <- unname(co[term, "Std. Error"])
  data.table(
    term = term,
    beta = beta,
    se = se,
    OR = exp(beta),
    CI_low = exp(beta - 1.96 * se),
    CI_high = exp(beta + 1.96 * se)
  )
}

## Build molecule factor (1 variable) inside a class
## Rules:
## - Determine which molecule flag columns exist for this class.
## - If none -> return NULL
## - If multiple flags are 1 in one case -> label "Multiple"
## - If none are 1 but class flag is 1 -> label "Unknown"
make_molecule_factor <- function(dt_in, cls_nm, class_flag, mol_in_cls) {
  tmp <- copy(dt_in)
  
  if (length(mol_in_cls) == 0) return(NULL)
  
  ## Count molecule flags per row
  tmp[, mol_count := rowSums(.SD, na.rm = TRUE), .SDcols = mol_in_cls]
  
  ## Determine molecule name for mol_count==1
  ## pick the first active flag (should be unique)
  tmp[, molecule := NA_character_]
  for (mc in mol_in_cls) {
    mol_name <- sub(paste0("^mol_", cls_nm, "__"), "", mc)
    tmp[get(mc) == 1 & mol_count == 1, molecule := mol_name]
  }
  
  ## Multiple / Unknown
  tmp[mol_count > 1, molecule := "Multiple"]
  tmp[mol_count == 0 & get(class_flag) == 1, molecule := "Unknown"]
  
  ## Keep only cases exposed to this class
  tmp <- tmp[get(class_flag) == 1]
  
  ## QC summary
  qc <- tmp[, .(N = .N, DILI = sum(DILI), rate = mean(DILI)), by = molecule][order(-N)]
  list(data = tmp, qc = qc)
}

## Choose reference molecule:
## Preference order: user-defined if provided; else the most frequent non-Unknown/non-Multiple.
pick_reference <- function(qc_dt, preferred = NULL) {
  if (!is.null(preferred) && preferred %in% qc_dt$molecule) return(preferred)
  cand <- qc_dt[!molecule %in% c("Unknown","Multiple")][order(-N)]
  if (nrow(cand) == 0) return(qc_dt[order(-N)]$molecule[1])
  cand$molecule[1]
}

## ---------------------------
## 1) Within-class models (FIXED)
## ---------------------------

within_results <- list()
within_qc_all  <- list()

for (cls in names(molecule_dict)) {
  
  cls_nm <- tolower(gsub("[^A-Za-z0-9]+","_", cls))
  class_flag <- paste0("flag_", cls_nm)
  
  if (!class_flag %in% names(dt)) {
    log_msg("Skip (missing class flag): ", cls)
    next
  }
  
  ## Molecule columns within this class
  mol_in_cls <- grep(paste0("^mol_", cls_nm, "__"), mol_cols, value = TRUE)
  if (length(mol_in_cls) < 1) {
    log_msg("Skip (no molecule cols): ", cls)
    next
  }
  
  ## Build molecule factor table
  out <- make_molecule_factor(dt, cls_nm, class_flag, mol_in_cls)
  if (is.null(out)) next
  
  dt_cls <- out$data
  qc_cls <- out$qc
  qc_cls[, drug_class := cls]
  within_qc_all[[cls]] <- qc_cls
  
  ## Exclude rows with missing sex/age (if any)
  dt_cls <- dt_cls[!is.na(sex_bin) & !is.na(age_bin)]
  
  ## Need at least 2 molecule levels with data (excluding Unknown/Multiple allowed but may complicate)
  if (length(unique(dt_cls$molecule)) < 2) {
    log_msg("Skip (only 1 molecule level): ", cls)
    next
  }
  
  ## Choose reference: for CNI we usually want Cyclosporine as reference (clinically meaningful)
  preferred_ref <- NULL
  if (cls == "Calcineurin inhibitors") preferred_ref <- "cyclosporine"
  if (cls == "Antiproliferative agents") preferred_ref <- "mycophenolate"
  if (cls == "mTOR inhibitors") preferred_ref <- "sirolimus"  # common baseline
  
  ref_mol <- pick_reference(qc_cls, preferred = preferred_ref)
  dt_cls[, molecule := factor(molecule)]
  ## ensure reference level first
  if (ref_mol %in% levels(dt_cls$molecule)) {
    dt_cls[, molecule := relevel(molecule, ref = ref_mol)]
  }
  
  ## Regimen adjustment: other class flags
  other_flags <- setdiff(flag_cols, class_flag)
  
  fml <- as.formula(paste(
    "DILI ~ sex_bin + age_bin +",
    paste(other_flags, collapse = " + "),
    "+ molecule"
  ))
  
  fit <- glm(fml, data = dt_cls, family = binomial())
  
  ## Extract all molecule coefficients (vs reference)
  co <- summary(fit)$coefficients
  coef_names <- rownames(co)
  mol_terms <- coef_names[grepl("^molecule", coef_names)]
  if (length(mol_terms) == 0) {
    log_msg("No molecule terms estimated for: ", cls, " (possible separation)")
    next
  }
  
  res <- data.table(
    drug_class = cls,
    model_type = "within_class_factor",
    reference_molecule = ref_mol,
    term = mol_terms,
    beta = co[mol_terms, "Estimate"],
    se   = co[mol_terms, "Std. Error"]
  )
  res[, OR := exp(beta)]
  res[, CI_low := exp(beta - 1.96 * se)]
  res[, CI_high := exp(beta + 1.96 * se)]
  
  within_results[[cls]] <- res
  saveRDS(fit, file.path(DIRS$MODEL, paste0("glm_within_factor_", cls_nm, ".rds")))
  
  log_msg("Fitted within-class factor model: ", cls,
          " | N=", nrow(dt_cls),
          " | ref=", ref_mol,
          " | levels=", length(levels(dt_cls$molecule)))
}

within_dt <- rbindlist(within_results, fill = TRUE)
within_qc_dt <- rbindlist(within_qc_all, fill = TRUE)

fwrite(within_qc_dt, file.path(DIRS$QC, "QC_within_class_molecule_distribution.csv"))

## ---------------------------
## 2) Direct contrast for CNI: Tacrolimus vs Cyclosporine
## ---------------------------

cni_contrast <- NULL
if ("Calcineurin inhibitors" %in% names(molecule_dict)) {
  
  cls <- "Calcineurin inhibitors"
  cls_nm <- tolower(gsub("[^A-Za-z0-9]+","_", cls))
  class_flag <- paste0("flag_", cls_nm)
  mol_in_cls <- grep(paste0("^mol_", cls_nm, "__"), mol_cols, value = TRUE)
  
  out <- make_molecule_factor(dt, cls_nm, class_flag, mol_in_cls)
  dt_cni <- out$data
  dt_cni <- dt_cni[molecule %in% c("tacrolimus", "cyclosporine")]
  dt_cni <- dt_cni[!is.na(sex_bin) & !is.na(age_bin)]
  
  if (nrow(dt_cni) > 0 && length(unique(dt_cni$molecule)) == 2) {
    
    dt_cni[, molecule := factor(molecule)]
    dt_cni[, molecule := relevel(molecule, ref = "cyclosporine")]
    
    other_flags <- setdiff(flag_cols, class_flag)
    
    fml_cni <- as.formula(paste(
      "DILI ~ sex_bin + age_bin +",
      paste(other_flags, collapse = " + "),
      "+ molecule"
    ))
    
    fit_cni <- glm(fml_cni, data = dt_cni, family = binomial())
    
    term <- "moleculetacrolimus"
    cni_contrast <- extract_or_ci(fit_cni, term)
    if (!is.null(cni_contrast)) {
      cni_contrast[, `:=`(
        drug_class = "Calcineurin inhibitors",
        model_type = "CNI_direct_contrast",
        comparison = "Tacrolimus vs Cyclosporine",
        reference_molecule = "cyclosporine"
      )]
      ## reorder cols
      setcolorder(cni_contrast, c("drug_class","model_type","comparison",
                                  "reference_molecule","term","beta","se","OR","CI_low","CI_high"))
      saveRDS(fit_cni, file.path(DIRS$MODEL, "glm_CNI_direct_contrast.rds"))
      log_msg("CNI direct contrast extracted: Tacrolimus vs Cyclosporine.")
    }
  } else {
    log_msg("CNI direct contrast skipped: insufficient data after filtering.")
  }
}

## ---------------------------
## 3) Global model (optional, factor-based across all molecules)
## ---------------------------
## NOTE: Global model is trickier because molecules from different classes are not mutually exclusive
## in combination regimens. To keep interpretation clean, we export within-class as primary.
## Here we keep your previous global model approach but remain cautious.

log_msg("Step6: Global model (dummy-based) kept as secondary output")

fml_global <- as.formula(paste(
  "DILI ~ sex_bin + age_bin +",
  paste(flag_cols, collapse=" + "), "+",
  paste(mol_cols, collapse=" + ")
))
fit_g <- glm(fml_global, data = dt[!is.na(sex_bin) & !is.na(age_bin)], family = binomial())
saveRDS(fit_g, file.path(DIRS$MODEL, "glm_global_molecule_dummy.rds"))

co_g <- summary(fit_g)$coefficients
coef_names_g <- rownames(co_g)
keep_g <- coef_names_g[grepl("^mol_", coef_names_g)]

global_dt <- data.table(
  drug_class = "ALL",
  model_type = "global_dummy_secondary",
  term = keep_g,
  beta = co_g[keep_g, "Estimate"],
  se   = co_g[keep_g, "Std. Error"]
)
global_dt[, OR := exp(beta)]
global_dt[, CI_low := exp(beta - 1.96 * se)]
global_dt[, CI_high := exp(beta + 1.96 * se)]

## ---------------------------
## 4) Save outputs
## ---------------------------

mol_or <- rbindlist(
  list(within_dt, cni_contrast, global_dt),
  fill = TRUE
)

fwrite(mol_or, file.path(DIRS$RESULTS, "molecule_heterogeneity_OR_CI.csv"))

log_msg("Step6 molecule models (FIXED) done. Results saved.")

