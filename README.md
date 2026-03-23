## Enhancing Mortality Prediction in Children with Sepsis in Tanzania: A Prospective Cohort Study Integrating Immune Biomarkers with Clinical Severity Scores
This repository contains code from my Master’s capstone project in Health Data Science at UCSF, where I independently led the study, including the data analysis, modeling, and manuscript preparation. The project evaluates whether host biomarkers improve prediction of in-hospital mortality in children with sepsis when combined with clinical severity scores.

## Project overview
In this project, I explored the relationship between clinical variables, biomarkers, and mortality outcomes. The goal was to evaluate whether adding biomarkers such as TREM-1, IL-8, and PCT improves predictive performance beyond standard clinical scores (LODS, qSOFA, SICK).

## What I did
- Cleaned and prepared a clinical dataset of pediatric sepsis patients  
- Generated baseline characteristics (Table 1) comparing survivors vs non-survivors  
- Evaluated biomarker performance using logistic regression and cross-validation (AUC)  
- Built a train/test pipeline to assess model performance on unseen data  
- Compared models using ΔAUC and Net Reclassification Improvement (NRI)  
- Used Random Forest and XGBoost to explore biomarker importance  

## Files
- `table1_and_table2.R` – baseline characteristics and descriptive analysis  
- `Biomarker_combinations.R` – cross-validated AUC analysis for biomarkers  
- `clinicalscores_biomarkers_table.R` – modeling with clinical scores + biomarkers  
- `feature_selection.R` – Random Forest and XGBoost feature importance  

## Tools
- R  
- tidyverse, gtsummary  
- pROC, caret  
- randomForest, xgboost  

## Note
This project was completed independently as part of my capstone research.
