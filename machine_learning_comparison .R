# ============================================================
# Compare logistic regression, LASSO, random forest, and GBM
# for mortality prediction using the same dataset and predictors
# ============================================================

library(caret)
library(dplyr)
library(pROC)
library(glmnet)
library(randomForest)
library(gbm)
library(openxlsx)

set.seed(123)

# ----------------------------
# 1) Load data
# ----------------------------
full_df <- read.csv("finalized_dataset.csv", stringsAsFactors = FALSE)
exclude_ids <- c(196, 351, 668, 884, 917)

full_df <- full_df %>%
  filter(!record_id %in% exclude_ids)

predictors <- c("lods_score", "log_trem1", "log_il8")

df <- full_df %>%
  select(mort_inhosp, all_of(predictors)) %>%
  filter(!is.na(mort_inhosp)) %>%
  na.omit()

df$mort_inhosp <- factor(df$mort_inhosp, levels = c("Died", "Survived"))

# ----------------------------
# 2) Train/test split
# ----------------------------
train_idx <- createDataPartition(df$mort_inhosp, p = 0.7, list = FALSE)
train_dat <- df[train_idx, ]
test_dat  <- df[-train_idx, ]

# ----------------------------
# 3) Cross-validation setup
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
# 4) Fit models
# ----------------------------

# Logistic regression
set.seed(123)
fit_glm <- train(
  x = train_dat[, predictors],
  y = train_dat$mort_inhosp,
  method = "glm",
  family = binomial,
  metric = "ROC",
  trControl = ctrl
)

# LASSO logistic regression
set.seed(123)
fit_lasso <- train(
  x = train_dat[, predictors],
  y = train_dat$mort_inhosp,
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
  x = train_dat[, predictors],
  y = train_dat$mort_inhosp,
  method = "rf",
  metric = "ROC",
  trControl = ctrl,
  tuneLength = 10,
  ntree = 1000
)

# GBM
set.seed(123)
fit_gbm <- train(
  x = train_dat[, predictors],
  y = train_dat$mort_inhosp,
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

# ----------------------------
# 5) Cross-validated AUC results
# ----------------------------
cv_results <- bind_rows(
  fit_glm$results %>%
    arrange(desc(ROC)) %>%
    slice(1) %>%
    mutate(Method = "Logistic Regression"),

  fit_lasso$results %>%
    arrange(desc(ROC), lambda) %>%
    slice(1) %>%
    mutate(Method = "LASSO"),

  fit_rf$results %>%
    arrange(desc(ROC)) %>%
    slice(1) %>%
    mutate(Method = "Random Forest"),

  fit_gbm$results %>%
    arrange(desc(ROC)) %>%
    slice(1) %>%
    mutate(Method = "GBM")
) %>%
  select(Method, ROC, Sens, Spec)

print(cv_results)

# ----------------------------
# 6) Test-set AUC helper
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
    Test_AUC_CI = sprintf(
      "%.3f (%.3f-%.3f)",
      as.numeric(auc(roc_obj)),
      ci[1],
      ci[3]
    )
  )
}

# ----------------------------
# 7) Test-set AUC results
# ----------------------------
test_results <- bind_rows(
  get_test_auc(fit_glm, test_dat)   %>% mutate(Method = "Logistic Regression"),
  get_test_auc(fit_lasso, test_dat) %>% mutate(Method = "LASSO"),
  get_test_auc(fit_rf, test_dat)    %>% mutate(Method = "Random Forest"),
  get_test_auc(fit_gbm, test_dat)   %>% mutate(Method = "GBM")
) %>%
  select(Method, Test_AUC, Test_AUC_CI)

print(test_results)

# ----------------------------
# 8) Combine CV and test results
# ----------------------------
final_compare <- cv_results %>%
  rename(CV_AUC = ROC) %>%
  left_join(test_results, by = "Method") %>%
  mutate(
    CV_AUC = round(CV_AUC, 3),
    Sens = round(Sens, 3),
    Spec = round(Spec, 3),
    Test_AUC = round(Test_AUC, 3)
  ) %>%
  arrange(desc(Test_AUC))

print(final_compare)

# ----------------------------
# 9) Export results
# ----------------------------
wb <- createWorkbook()

addWorksheet(wb, "CV_Results")
writeData(wb, "CV_Results", cv_results)

addWorksheet(wb, "Test_Results")
writeData(wb, "Test_Results", test_results)

addWorksheet(wb, "Final_Comparison")
writeData(wb, "Final_Comparison", final_compare)

saveWorkbook(wb, "Method_Comparison_LogReg_LASSO_RF_GBM.xlsx", overwrite = TRUE)

cat("\nSaved: Method_Comparison_LogReg_LASSO_RF_GBM.xlsx\n")
