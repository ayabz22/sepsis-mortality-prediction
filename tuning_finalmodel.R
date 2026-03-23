library(caret)
library(dplyr)

# -------------------------------
# 1) Prepare data (TRAIN ONLY)
# -------------------------------
vars <- c("outcome", "lods_score", "log_trem1", "log_il8")

train_model_df <- df[complete.cases(df[, vars]), vars]

# -------------------------------
# 2) Fit final model on TRAIN
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
# 4) Loop through thresholds
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

# -------------------------------
# 5) View full threshold list
# -------------------------------
head(results, 20)      # first 20 thresholds
tail(results, 20)      # last 20 thresholds

# -------------------------------
# 6) Sort by different criteria
# -------------------------------

# Highest Youden
results %>% arrange(desc(youden)) %>% head(10)

# Highest sensitivity
results %>% arrange(desc(sensitivity)) %>% head(10)

# Highest PPV
results %>% arrange(desc(PPV)) %>% head(10)


#0.13 was picked 
# ============================================================
# 5) Make sure outcome exists in TEST
# ============================================================
test_df$outcome <- ifelse(test_df$mort_inhosp == "Died", 1, 0)

# ============================================================
# 6) Prepare TEST data (complete cases only)
# ============================================================
vars <- c("outcome", "lods_score", "log_trem1", "log_il8")

test_model_df <- test_df[complete.cases(test_df[, vars]), vars]

# Predict probabilities on TEST
p_test <- predict(fit, newdata = test_model_df, type = "response")

# Sanity check
length(p_test)
nrow(test_model_df)
length(test_model_df$outcome)

# ============================================================
# 7) Helper function to compute metrics at a given threshold
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
# 8) Evaluate chosen threshold on TEST
# ============================================================
test_perf <- get_metrics(0.13, p_test, test_model_df$outcome)
test_perf

# ============================================================
# 9) Sensitivity analysis on TEST: compare nearby thresholds
# ============================================================
thresholds_to_check <- c(0.10, 0.12, 0.13, 0.15)

sens_analysis <- do.call(
  rbind,
  lapply(thresholds_to_check, function(t) {
    get_metrics(t, p_test, test_model_df$outcome)
  })
)

sens_analysis

# ============================================================
# 10) Report how many TEST rows were dropped
# ============================================================
cat("Test rows total:", nrow(test_df), "\n")
cat("Test rows used (complete cases):", nrow(test_model_df), "\n")
cat("Test rows dropped due to missing predictors/outcome:",
    nrow(test_df) - nrow(test_model_df), "\n")








