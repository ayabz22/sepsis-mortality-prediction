# ============================================================
# Comparing different models for mortality prediction
#
# Here I'm testing logistic regression, LASSO, random forest,
# and GBM across different combinations of clinical scores
# and biomarkers to see which approach performs best.
# I use cross-validation on the training set and then check
# performance on the test set.
# ============================================================

library(caret)
library(dplyr)
library(pROC)
library(glmnet)
library(randomForest)
library(gbm)
library(openxlsx)
library(tibble)

set.seed(123)

# ----------------------------
# 1) Load data
# ----------------------------
full_df <- read.csv("finalized_dataset.csv", stringsAsFactors = FALSE)
exclude_ids <- c(196, 351, 668, 884, 917)

full_df <- full_df %>%
  filter(!record_id %in% exclude_ids)

combinations <- list(
  c("lods_score", "log_trem1", "log_il8"),
  c("lods_score", "log_trem1", "log_pct", "log_il8"),
  c("lods_score", "log_trem1"),
  c("lods_score", "log_trem1", "log_pct"),
  c("lods_score", "log_pct", "log_il8"),
  c("lods_score", "log_il8"),
  c("lods_score", "log_pct"),
  c("qsofa", "log_trem1", "log_pct", "log_il8"),
  c("qsofa", "log_trem1", "log_il8"),
  c("qsofa", "log_trem1", "log_pct"),
  c("qsofa", "log_trem1"),
  c("qsofa", "log_pct", "log_il8"),
  c("qsofa", "log_il8"),
  c("qsofa", "log_pct"),
  c("sick_score", "log_trem1", "log_pct", "log_il8"),
  c("sick_score", "log_trem1"),
  c("sick_score", "log_trem1", "log_il8"),
  c("sick_score", "log_trem1", "log_pct"),
  c("sick_score", "log_pct", "log_il8"),
  c("sick_score", "log_il8"),
  c("sick_score", "log_pct")
)

df <- full_df %>%
  filter(!is.na(mort_inhosp))

df$mort_inhosp <- factor(df$mort_inhosp, levels = c("Died", "Survived"))

# ----------------------------
# 2) Split into training and test sets (70/30)
# ----------------------------
train_idx <- createDataPartition(df$mort_inhosp, p = 0.7, list = FALSE)
train_dat <- df[train_idx, ]
test_dat  <- df[-train_idx, ]

# ----------------------------
# 3) Set up cross-validation (same for all models)
# ----------------------------
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# ----------------------------
# Helper function to calculate test AUC 
# ----------------------------
get_test_auc <- function(fit, newdata, outcome_name = "mort_inhosp", positive = "Died") {
  probs <- predict(fit, newdata = newdata, type = "prob")[, positive]
  
  roc_obj <- roc(
    response = newdata[[outcome_name]],
    predictor = probs,
    levels = c("Survived", "Died"),
    direction = "<"
  )
  
  ci <- ci.auc(roc_obj)
  
  tibble(
    Test_AUC = as.numeric(auc(roc_obj)),
    Test_AUC_CI = sprintf("%.3f (%.3f-%.3f)", as.numeric(auc(roc_obj)), ci[1], ci[3])
  )
}

# ----------------------------
# 4) Fit models for each feature combination
# ----------------------------
cv_results <- list()
test_results <- list()
model_store <- list()

for (comb in combinations) {
  
  combo_name <- paste(comb, collapse = "_")

  missing_vars <- setdiff(c(comb, "mort_inhosp"), names(train_dat))
  if (length(missing_vars) > 0) {
    message("Skipping ", combo_name, " because these columns are missing: ",
            paste(missing_vars, collapse = ", "))
    next
  }
  
  train_sub <- train_dat %>%
    select(all_of(c("mort_inhosp", comb))) %>%
    na.omit()
  
  test_sub <- test_dat %>%
    select(all_of(c("mort_inhosp", comb))) %>%
    na.omit()

  if (nrow(train_sub) < 20 || nrow(test_sub) < 10) {
    message("Skipping ", combo_name, " due to too few complete cases.")
    next
  }
  
  if (length(unique(train_sub$mort_inhosp)) < 2 || length(unique(test_sub$mort_inhosp)) < 2) {
    message("Skipping ", combo_name, " because one class is missing in train or test.")
    next
  }
  
  x_train <- train_sub[, comb, drop = FALSE]
  y_train <- train_sub$mort_inhosp
  
  # Logistic regression
  set.seed(123)
  fit_glm <- train(
    x = x_train,
    y = y_train,
    method = "glm",
    family = binomial(),
    metric = "ROC",
    trControl = ctrl
  )
  
  # LASSO
  set.seed(123)
  fit_lasso <- train(
    x = x_train,
    y = y_train,
    method = "glmnet",
    metric = "ROC",
    trControl = ctrl,
    tuneGrid = expand.grid(
      alpha = 1,
      lambda = 10^seq(-4, 1, length.out = 50)
    )
  )
  
  # Random forest
  set.seed(123)
  fit_rf <- train(
    x = x_train,
    y = y_train,
    method = "rf",
    metric = "ROC",
    trControl = ctrl,
    tuneLength = 10,
    ntree = 1000
  )
  
  # GBM
  set.seed(123)
  fit_gbm <- train(
    x = x_train,
    y = y_train,
    method = "gbm",
    metric = "ROC",
    trControl = ctrl,
    verbose = FALSE,
    tuneGrid = expand.grid(
      interaction.depth = c(1, 2, 3),
      n.trees = c(50, 100, 150, 200),
      shrinkage = c(0.01, 0.1),
      n.minobsinnode = 10
    )
  )
  
  model_store[[combo_name]] <- list(
    glm = fit_glm,
    lasso = fit_lasso,
    rf = fit_rf,
    gbm = fit_gbm
  )
  
  # Best CV results
  cv_results[[combo_name]] <- bind_rows(
    fit_glm$results %>%
      slice_max(order_by = ROC, n = 1, with_ties = FALSE) %>%
      mutate(Method = "Logistic Regression", Combination = combo_name),
    
    fit_lasso$results %>%
      slice_max(order_by = ROC, n = 1, with_ties = FALSE) %>%
      mutate(Method = "LASSO", Combination = combo_name),
    
    fit_rf$results %>%
      slice_max(order_by = ROC, n = 1, with_ties = FALSE) %>%
      mutate(Method = "Random Forest", Combination = combo_name),
    
    fit_gbm$results %>%
      slice_max(order_by = ROC, n = 1, with_ties = FALSE) %>%
      mutate(Method = "GBM", Combination = combo_name)
  )
  
  # Test-set AUC for THIS combo's models
  test_results[[combo_name]] <- bind_rows(
    get_test_auc(fit_glm, test_sub)   %>%
      mutate(Method = "Logistic Regression", Combination = combo_name),
    
    get_test_auc(fit_lasso, test_sub) %>%
      mutate(Method = "LASSO", Combination = combo_name),
    
    get_test_auc(fit_rf, test_sub)    %>%
      mutate(Method = "Random Forest", Combination = combo_name),
    
    get_test_auc(fit_gbm, test_sub)   %>%
      mutate(Method = "GBM", Combination = combo_name)
  )
}

# ----------------------------
# 5) Combine results
# ----------------------------
cv_results_df <- bind_rows(cv_results)
test_results_df <- bind_rows(test_results)

final_compare <- cv_results_df %>%
  left_join(test_results_df, by = c("Method", "Combination")) %>%
  arrange(desc(Test_AUC))

print(final_compare)

# ----------------------------
# 6) Export results
# ----------------------------
wb <- createWorkbook()

addWorksheet(wb, "CV_Results")
writeData(wb, "CV_Results", cv_results_df)

addWorksheet(wb, "Test_Results")
writeData(wb, "Test_Results", test_results_df)

addWorksheet(wb, "Final_Comparison")
writeData(wb, "Final_Comparison", final_compare)

saveWorkbook(wb, "Model_Comparison_AUC_Results.xlsx", overwrite = TRUE)
