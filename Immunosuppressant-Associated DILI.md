# Immunosuppressant-Associated DILI  
## Study Design, Analytical Workflow, and Key Findings

---

## 1. Study Objective and Overall Rationale

Drug-induced liver injury (DILI) is a clinically important but heterogeneous adverse event associated with immunosuppressive therapies. While immunosuppressants are often treated as homogeneous drug classes in pharmacovigilance studies, accumulating evidence suggests that DILI risk may vary substantially across drug classes, treatment regimens, and individual molecules.

This study aimed to:

- Systematically characterize immunosuppressant-associated DILI using large-scale real-world pharmacovigilance data.
- Quantify DILI risk at multiple hierarchical levels (class → regimen → molecule).
- Identify stable and clinically interpretable risk determinants using regularized modeling.
- Validate findings in an independent external database.

---

## 2. Data Sources and Study Scope

### 2.1 Pharmacovigilance Databases

- **FAERS (FDA Adverse Event Reporting System)**  
  Primary discovery dataset (2004–2024), used for signal detection, modeling, risk portrait construction, and molecular heterogeneity analysis.

- **JADER (Japanese Adverse Drug Event Report Database)**  
  External validation dataset, used to evaluate reproducibility of reporting characteristics, model performance, and calibration.

### 2.2 Drug Classes of Interest

Five major immunosuppressant classes were pre-specified:

1. Calcineurin inhibitors  
2. Antiproliferative agents  
3. mTOR inhibitors  
4. Corticosteroids  
5. Biologics  

All downstream analyses were anchored to these five classes.

---

## 3. Study Workflow Overview (Figure 1)

The analytical workflow followed a top-down hierarchical structure:

1. Descriptive characterization of immunosuppressant-associated DILI reports  
2. Hierarchical hepatobiliary signal detection  
3. Multivariable risk modeling and feature selection  
4. Construction of population-level risk portraits  
5. External validation using JADER  
6. Molecular-level heterogeneity analysis  

Each step informed and constrained the next, avoiding circular inference.

---

## 4. Descriptive Characteristics of DILI Reports

### 4.1 FAERS: Immunosuppressant-Associated DILI Characteristics (Figure 2)

- Annual reporting volumes varied substantially across drug classes.
- Corticosteroids and biologics contributed the largest absolute number of DILI reports.
- Sex distributions were broadly comparable across classes, with modest male predominance in selected categories.
- Age distributions demonstrated class-specific patterns, reflecting underlying clinical indications.

### 4.2 JADER: External Descriptive Validation (Figure 5)

- A total of 80,362 reports involving the five immunosuppressant classes were identified.
- Annual reporting patterns and demographic structures closely mirrored FAERS.
- Class contribution ranking, sex balance, and age distributions were highly concordant between databases.

---

## 5. Hierarchical Hepatobiliary Signal Detection (Figure 3)

Hepatobiliary adverse events were examined using a MedDRA hierarchy-aware framework:

- SOC: Hepatobiliary disorders  
- HLGT → HLT → PT  

Key observations:

- Immunosuppressant-associated DILI signals were consistently enriched across multiple hierarchical levels.
- Signal structures were coherent rather than driven by isolated preferred terms.
- This confirmed that downstream modeling was grounded in genuine hepatobiliary risk patterns.

---

## 6. Multivariable Risk Modeling and Feature Selection (Figure 4)

### 6.1 Modeling Strategy

Three complementary models were constructed:

- **Model_Full:** LASSO-regularized logistic regression including demographics and all drug classes  
- **Model_P:** Parsimonious LASSO model (λ₁se)  
- **Model_E:** Standard logistic refit using selected predictors  

### 6.2 Key Findings

- LASSO selection identified drug class variables as stable, non-redundant contributors to DILI risk.
- Model discrimination was modest (AUROC ≈ 0.53–0.54), consistent with spontaneous reporting data.
- Calibration and Brier score analysis confirmed internally coherent risk estimation.

---

## 7. Population-Level Risk Portraits (Figure 6)

### 7.1 Concept

Risk portraits integrate sex, age group, and immunosuppressant treatment regimen to produce combinatorial risk estimates.

### 7.2 Key Results

- DILI risk exhibited strong regimen-driven heterogeneity.
- Regimens combining calcineurin inhibitors, antiproliferative agents, and corticosteroids consistently occupied the highest risk strata.
- Very-high-risk profiles clustered in older age groups but were not exclusive to them.

---

## 8. External Validation in JADER

- FAERS-derived models were applied to JADER without re-training.
- Discrimination was reduced but risk ranking was preserved.
- Calibration-in-the-large demonstrated mild under-prediction without systematic deviation.

---

## 9. Molecular-Level Heterogeneity Analysis (Figure 7)

### 9.1 Rationale

After establishing class-level and regimen-level risk, analyses were extended to explore within-class molecular heterogeneity.

### 9.2 Analytical Approach

- Molecule-level comparisons were conducted within each immunosuppressant class.
- Odds ratios were estimated relative to a reference molecule from the same class.
- This represented a within-class contrast analysis rather than repeated signal detection.

### 9.3 Key Findings

- Substantial within-class heterogeneity was observed.
- A limited number of molecules consistently exhibited markedly elevated DILI risk relative to class peers.
- Class-level DILI signals were partially driven by specific high-risk agents.

---

## 10. Overall Conclusions

- Immunosuppressant-associated DILI shows pronounced heterogeneity across classes, regimens, and molecules.
- Regularized modeling enables stable identification of meaningful risk contributors.
- Risk portraits translate statistical associations into clinically interpretable patterns.
- Molecular heterogeneity analysis reveals that class-level risk may be dominated by specific high-risk agents.
- External validation confirms robustness and generalizability of the analytical framework.

---

## 11. Methodological Significance

This study provides a scalable, hierarchy-aware, and clinically interpretable framework for pharmacovigilance research that integrates signal detection, risk modeling, population stratification, and molecular-level interpretation.
