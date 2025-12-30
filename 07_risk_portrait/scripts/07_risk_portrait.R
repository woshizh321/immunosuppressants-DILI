## =========================================================
## Regimen-aware risk portrait (class-level)
## =========================================================

library(data.table)
library(here)

## ---------------------------
## 0) Load Model_E coefficients
## ---------------------------

coef_path <- here(
  "04_model_E_logistic",
  "outputs", "evaluation",
  "OR_CI.csv"
)
stopifnot(file.exists(coef_path))

coef_dt <- fread(coef_path)

## log(OR) as additive contribution
coef_dt[, beta := log(OR)]

# beta lookup map: names are terms, values are betas
beta_map <- setNames(coef_dt$beta, coef_dt$term)

# helper: map term vector -> beta vector, missing -> 0
map_beta <- function(term_vec) {
  out <- beta_map[term_vec]
  out[is.na(out)] <- 0
  as.numeric(out)
}

cat("QC: Example betas\n")
print(coef_dt[term %in% c("sex_binMale","sex_binOther","age_bin65+",
                          "flag_calcineurin_inhibitors"), .(term, OR, beta)])


## ---------------------------
## 1) Demographic grid
## ---------------------------

sex_levels <- c("Female", "Male", "Other")
age_levels <- c("<18", "18-39", "40-64", "65+")

demo_grid <- CJ(
  sex = sex_levels,
  age = age_levels
)

## ---------------------------
## 2) Regimen components
## ---------------------------

drug_classes <- c(
  "calcineurin_inhibitors",
  "antiproliferative_agents",
  "mtor_inhibitors",
  "corticosteroids",
  "biologics"
)

core_classes <- setdiff(drug_classes, "corticosteroids")

## Generate clinically plausible regimens
generate_regimens <- function(core, cs) {
  regs <- list()
  
  ## single core
  for (x in core) {
    regs[[length(regs) + 1]] <- c(x)
    regs[[length(regs) + 1]] <- c(x, cs)
  }
  
  ## two cores
  cmb <- combn(core, 2, simplify = FALSE)
  for (x in cmb) {
    regs[[length(regs) + 1]] <- x
    regs[[length(regs) + 1]] <- c(x, cs)
  }
  
  unique(lapply(regs, sort))
}

regimen_list <- generate_regimens(core_classes, "corticosteroids")

## ---------------------------
## 3) Build portrait grid
## ---------------------------

portrait <- rbindlist(lapply(regimen_list, function(reg) {
  cbind(
    demo_grid,
    regimen = paste(reg, collapse = "+")
  )
}), fill = TRUE)

## ---------------------------
## 4) Compute risk score
## ---------------------------

portrait[, logOR_combo := 0]

# sex
portrait[, logOR_combo := logOR_combo + map_beta(paste0("sex_bin", sex))]

# age
portrait[, logOR_combo := logOR_combo + map_beta(paste0("age_bin", age))]

# drug classes (add beta if class present in regimen)
for (cls in drug_classes) {
  term <- paste0("flag_", cls)
  b <- map_beta(term) # scalar (length 1) but safe
  portrait[grepl(cls, regimen), logOR_combo := logOR_combo + b]
}


portrait[, OR_combo := exp(logOR_combo)]

cat("QC: logOR_combo summary\n")
print(summary(portrait$logOR_combo))

cat("QC: OR_combo summary\n")
portrait[, OR_combo := exp(logOR_combo)]
print(summary(portrait$OR_combo))

## ---------------------------
## 5) Risk categorization
## ---------------------------

qs <- quantile(portrait$OR_combo,
               probs = c(0.25, 0.5, 0.75))

portrait[, risk_cat := fifelse(
  OR_combo <= qs[1], "Low",
  fifelse(OR_combo <= qs[2], "Moderate",
          fifelse(OR_combo <= qs[3], "High", "Very high"))
)]

setorder(portrait, -OR_combo)

## ---------------------------
## 6) Save outputs
## ---------------------------

out_path <- here(
  "07_risk_portrait",
  "outputs", "tables",
  "regimen_risk_portrait_table.csv"
)

fwrite(portrait, out_path)

cat("Step5 regimen-aware risk portrait saved:\n", out_path, "\n")
