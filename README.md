## Enhancing Mortality Prediction in Children with Sepsis in Tanzania: A Prospective Cohort Study Integrating Immune Biomarkers with Clinical Severity Scores

This repository contains code from my Master’s capstone project in Health Data Science at UCSF, where I independently led the study, including data analysis, modeling, and manuscript preparation. The project evaluates whether host biomarkers improve prediction of in-hospital mortality in children with sepsis when combined with clinical severity scores.

## Project overview
I investigated the relationship between clinical variables, immune biomarkers, and mortality outcomes. The primary goal was to determine whether biomarkers such as TREM-1, IL-8, and PCT improve predictive performance beyond standard clinical severity scores (LODS, qSOFA, and SICK).

## Analysis workflow

- `table1_and_table2.R`  
  Generates baseline characteristics and biomarker summaries comparing survivors vs non-survivors.

- `Biomarker_combinations.R`  
  Evaluates predictive performance of biomarkers alone and in combination with clinical scores using cross-validated AUC.

- `clinicalscores_biomarkers_table.R`  
  Builds final logistic regression models combining clinical scores and biomarkers.

- `tuning_finalmodel.R`  
  Implements train/test validation, evaluates model performance on unseen data, and computes metrics such as ΔAUC and NRI.

- `machine_learning_comparison.R`  
  Compares logistic regression, LASSO, random forest, and gradient boosting models using cross-validation and test-set AUC.

- `feature_selection.R`  
  Uses random forest and gradient boosting to assess biomarker importance.

