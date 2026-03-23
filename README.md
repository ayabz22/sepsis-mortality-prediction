## Enhancing Mortality Prediction in Children with Sepsis in Tanzania: A Prospective Cohort Study Integrating Immune Biomarkers with Clinical Severity Scores

This repository contains code from my Master’s capstone project in Health Data Science at UCSF, where I independently led the study, including data analysis, modeling, and manuscript preparation. The project evaluates whether host biomarkers improve prediction of in-hospital mortality in children with sepsis when combined with clinical severity scores.

## Project overview
I investigated the relationship between clinical variables, immune biomarkers, and mortality outcomes. The primary goal was to determine whether biomarkers such as TREM-1, IL-8, and PCT improve predictive performance beyond standard clinical severity scores (LODS, qSOFA, and SICK).

## Analysis workflow

- `table1_and_table2.R`  
  Generates baseline clinical characteristics and biomarker summaries by survival status, including statistical comparisons between groups.

- `clinicalscores_biomarkers_table.R`  
 Evaluates biomarker predictive performance using 5-fold cross-validated AUC (with 95% CI), comparing biomarkers alone and in combination with clinical scores (qSOFA, LODS, SICK), and generates a summary results table for reporting.

- `machine_learning_comparison.R`  
  Compares logistic regression, LASSO, random forest, and gradient boosting across different biomarker and clinical score combinations using cross-validation and test-set AUC.

- `Biomarker_combinations.R`  
  Selects optimal biomarker combinations using cross-validated AUC on training data, evaluates final model performance on the test set using AUC, ΔAUC, and NRI, and generates ROC curves comparing training and testing performance.

- `tuning_finalmodel.R`
  Evaluates probability thresholds for the final logistic regression model and performs sensitivity analysis to assess how classification performance varies across nearby thresholds.

- `feature_selection.R`  
  Uses random forest and gradient boosting to assess biomarker importance.

