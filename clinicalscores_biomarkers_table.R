# Evaluate predictive performance of biomarkers for in-hospital mortality
# Compare biomarkers alone vs combined with clinical scores (qSOFA, LODS, SICK)
# Use 5-fold cross-validation and report AUC with 95% CI

library(caret)
library(dplyr)
library(pROC)
library(tibble)
library(boot)
library(flextable)
library(officer)

set.seed(123)
full_df <-  read.csv("finalized_dataset.csv", stringsAsFactors = FALSE)
exclude_ids <- c(196, 351, 668, 884, 917)

full_df <- full_df %>%
  filter(!record_id %in% exclude_ids)

train_idx <- createDataPartition(
  full_df$mort_inhosp,
  p = 0.7,
  list = FALSE
)

df <- full_df[train_idx, ]
test_df  <- full_df[-train_idx, ]
df <- df %>%
  mutate(outcome = mort_inhosp)
test_df <- test_df %>%
  mutate(outcome = mort_inhosp)


# =============================
# Define biomarkers and clinical scores used in the analysis
# =============================
biomarkers <- c(
  "log_il10", "log_ang2", "log_il6", "log_il1ra", "log_il8",
  "log_tnfr1", "log_proc", "log_rage", "log_pai1", "log_icam1",
  "log_trem1", "log_crp", "log_fer", "log_pct",
  "log_lact", "log_hco3"
)

clinical_scores <- c("qsofa", "lods_score", "sick_score")

# Outcome must be 0/1 for logistic regression
df$outcome <- ifelse(df$mort_inhosp == "Died", 1, 0)

# Number of folds for cross-validation
k <- 5
set.seed(123)

# =============================
# Function to compute cross-validated AUC with 95% CI
# =============================
cv_auc_ci <- function(formula_str, data, k = 5) {
  vars_needed <- all.vars(as.formula(formula_str))
  
  if (!all(vars_needed %in% colnames(data))) return(NA)
  
  dat <- data[, vars_needed, drop = FALSE] %>% na.omit()
  if (length(unique(dat$outcome)) < 2) return(NA)
  
  folds <- sample(rep(1:k, length.out = nrow(dat)))
  aucs <- numeric(k)
  
  for (i in 1:k) {
    train <- dat[folds != i, ]
    test  <- dat[folds == i, ]
    
    if (length(unique(train$outcome)) < 2 || length(unique(test$outcome)) < 2) next
    
    fit <- glm(as.formula(formula_str), data = train, family = binomial)
    preds <- predict(fit, test, type = "response")
    
    roc_obj <- roc(test$outcome, preds, quiet = TRUE)
    aucs[i] <- as.numeric(auc(roc_obj))
  }
  
  mean_auc <- mean(aucs, na.rm = TRUE)
  se_auc   <- sd(aucs, na.rm = TRUE) / sqrt(sum(!is.na(aucs)))
  
  sprintf("%.3f (%.3f–%.3f)", mean_auc, mean_auc - 1.96*se_auc, mean_auc + 1.96*se_auc)
}

# =============================
# Loop through biomarkers and compute AUCs
# =============================
final_table <- tibble()

for (marker in biomarkers) {
  # Use same patients for all models in this row
  tmp <- df %>%
    select(outcome, all_of(marker), all_of(clinical_scores)) %>%
    na.omit()
  
  if (length(unique(tmp$outcome)) < 2) next
  
  row <- tibble(
    Biomarker = marker,
    Alone_CV_AUC = cv_auc_ci(paste("outcome ~", marker), tmp, k),
    AUC_qSOFA = cv_auc_ci(paste("outcome ~ qsofa +", marker), tmp, k),
    AUC_LODS  = cv_auc_ci(paste("outcome ~ lods_score +", marker), tmp, k),
    AUC_Sick  = cv_auc_ci(paste("outcome ~ sick_score +", marker), tmp, k),
    N = nrow(tmp)
  )
  
  final_table <- bind_rows(final_table, row)
}

# Sort by Alone CV AUC
final_table <- final_table %>%
  arrange(desc(Alone_CV_AUC))

final_table <- final_table %>%
  mutate(Biomarker = case_when(
    Biomarker == "log_il10"  ~ "IL-10",
    Biomarker == "log_ang2"  ~ "Angiopoietin-2",
    Biomarker == "log_il6"   ~ "IL-6",
    Biomarker == "log_il1ra" ~ "IL-1RA",
    Biomarker == "log_il8"   ~ "IL-8",
    Biomarker == "log_tnfr1" ~ "TNFR1",
    Biomarker == "log_proc"  ~ "Protein C",
    Biomarker == "log_rage"  ~ "sRAGE",
    Biomarker == "log_pai1"  ~ "PAI-1",
    Biomarker == "log_icam1" ~ "ICAM-1",
    Biomarker == "log_trem1" ~ "TREM-1",
    Biomarker == "log_crp"   ~ "CRP",
    Biomarker == "log_fer"   ~ "Ferritin",
    Biomarker == "log_pct"   ~ "Procalcitonin",
    Biomarker == "log_lact"  ~ "Lactate",
    Biomarker == "log_hco3"  ~ "Bicarbonate",
    TRUE ~ Biomarker
  ))
print(final_table)


library(openxlsx)

final_xlsx <- final_table %>%
  dplyr::rename(
    `Biomarker` = Biomarker,
    `Alone CV AUC (95% CI)` = Alone_CV_AUC,
    `+ qSOFA CV AUC (95% CI)` = AUC_qSOFA,
    `+ LODS CV AUC (95% CI)` = AUC_LODS,
    `+ Sick Score CV AUC (95% CI)` = AUC_Sick,
    `Sample Size` = N
  )

wb <- createWorkbook()
addWorksheet(wb, "Table")

writeData(wb, "Table", final_xlsx)

header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE)
addStyle(wb, "Table", header_style, rows = 1, cols = 1:ncol(final_xlsx), gridExpand = TRUE)

center_style <- createStyle(halign = "center", valign = "center", wrapText = TRUE)
addStyle(wb, "Table", center_style, rows = 2:(nrow(final_xlsx) + 1), cols = 1:ncol(final_xlsx), gridExpand = TRUE)

freezePane(wb, "Table", firstRow = TRUE)
setColWidths(wb, "Table", cols = 1:ncol(final_xlsx), widths = "auto")

saveWorkbook(wb, "CV_AUC_Biomarkers_Table.xlsx", overwrite = TRUE)

