# ============================================================
# Threshold Selection and Test-Set Evaluation
#
# This script fits the final logistic regression model on the
# training set, evaluates candidate probability thresholds,
# and then assesses the selected threshold on the test set (sensitivity analysis).
# ============================================================

library(caret)
library(dplyr)

# -------------------------------
# 1) Prepare training data 
# -------------------------------
vars <- c("outcome", "lods_score", "log_trem1", "log_il8")

train_model_df <- df[complete.cases(df[, vars]), vars]

# -------------------------------
# 2) Fit final model on training data 
# -------------------------------
fit <- glm(outcome ~ lods_score + log_trem1 + log_il8,
           data = train_model_df,
           family = binomial)

p_train <- predict(fit, type = "response")

# -------------------------------
# 3) Threshold grid
# -------------------------------
thresholds <- seq(0, 1, by = 0.01)

results <- data.frame(
  threshold = thresholds,
  sensitivity = NA_real_,
  specificity = NA_real_,
  PPV = NA_real_,
  NPV = NA_real_,
  youden = NA_real_
)

# -------------------------------
# 4) Evaluate training performance across thresholds 
# -------------------------------
for (i in seq_along(thresholds)) {
  
  t <- thresholds[i]
  pred_class <- ifelse(p_train >= t, 1, 0)
  
  cm <- confusionMatrix(
    factor(pred_class, levels = c(0,1)),
    factor(train_model_df$outcome, levels = c(0,1)),
    positive = "1"
  )
  
  sens <- unname(cm$byClass["Sensitivity"])
  spec <- unname(cm$byClass["Specificity"])
  
  results$sensitivity[i] <- sens
  results$specificity[i] <- spec
  results$PPV[i] <- unname(cm$byClass["Pos Pred Value"])
  results$NPV[i] <- unname(cm$byClass["Neg Pred Value"])
  results$youden[i] <- sens + spec - 1
}

#Evaluate selected probability threshold 

# ============================================================
# 5) Prepare TEST data
# ============================================================
vars <- c("outcome", "lods_score", "log_trem1", "log_il8")

test_model_df <- test_df[complete.cases(test_df[, vars]), vars]

# predict probabilities on TEST
p_test <- predict(fit, newdata = test_model_df, type = "response")

# ============================================================
# 6) Helper function to calculate metrics at a given threshold
# ============================================================
get_metrics <- function(threshold, probs, outcome) {
  
  pred_class <- ifelse(probs >= threshold, 1, 0)
  
  cm <- confusionMatrix(
    factor(pred_class, levels = c(0,1)),
    factor(outcome, levels = c(0,1)),
    positive = "1"
  )
  
  data.frame(
    threshold   = threshold,
    sensitivity = unname(cm$byClass["Sensitivity"]),
    specificity = unname(cm$byClass["Specificity"]),
    PPV         = unname(cm$byClass["Pos Pred Value"]),
    NPV         = unname(cm$byClass["Neg Pred Value"])
  )
}

# ============================================================
#7) Evaluate chosen threshold on TEST
# ============================================================
test_perf <- get_metrics(0.13, p_test, test_model_df$outcome)
test_perf

# ============================================================
# 8) Sensitivity analysis on TEST for nearby thresholds
# ============================================================
thresholds_to_check <- c(0.10, 0.12, 0.13, 0.15)

sens_analysis <- do.call(
  rbind,
  lapply(thresholds_to_check, function(t) {
    get_metrics(t, p_test, test_model_df$outcome)
  })
)

sens_analysis
