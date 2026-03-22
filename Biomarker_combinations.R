# ============================================================
# FIXED PIPELINE:
# - Select biomarker combos using CV on TRAIN only
# - Fit final models on FULL TRAIN only
# - Evaluate on TEST only (NO refitting on test)
# ============================================================

library(tidyverse)
library(pROC)
library(boot)
library(openxlsx)

# ============================================================
# 1) Outcome coding (0/1)
# ============================================================
df$outcome      <- ifelse(df$mort_inhosp == "Died", 1, 0)
test_df$outcome <- ifelse(test_df$mort_inhosp == "Died", 1, 0)

# ============================================================
# 2) Predictors
# ============================================================
biomarkers <- c("log_trem1", "log_pct", "log_il8")
clinical_scores <- c("qsofa", "sick_score", "lods_score")

bio_label <- function(x) {
  dplyr::case_when(
    x == "log_trem1" ~ "TREM-1",
    x == "log_pct"   ~ "PCT",
    x == "log_il8"   ~ "IL-8",
    TRUE ~ x
  )
}

# ============================================================
# 3) Cross-validated AUC (TRAINING ONLY)
#    Fits within folds on train_k and evaluates on valid_k
# ============================================================
cv_auc_ci <- function(formula, data, K = 5, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  folds <- sample(rep(1:K, length.out = n))
  aucs <- numeric(K)
  
  for (k in 1:K) {
    train_k <- data[folds != k, ]
    valid_k <- data[folds == k, ]
    
    fit <- glm(formula, data = train_k, family = binomial)
    preds <- predict(fit, newdata = valid_k, type = "response")
    roc_obj <- pROC::roc(valid_k$outcome, preds, quiet = TRUE)
    aucs[k] <- as.numeric(pROC::auc(roc_obj))
  }
  
  mean_auc <- mean(aucs)
  se_auc   <- sd(aucs) / sqrt(K)
  
  lower <- max(0, mean_auc - 1.96 * se_auc)
  upper <- min(1, mean_auc + 1.96 * se_auc)
  
  list(
    auc = mean_auc,
    lower = lower,
    upper = upper,
    auc_ci = sprintf("%.3f (%.3f–%.3f)", mean_auc, lower, upper),
    fold_aucs = aucs
  )
}

# ============================================================
# 4) Candidate formulas
# ============================================================
biomarker_pairs <- list(
  c("log_trem1", "log_pct"),
  c("log_trem1", "log_il8"),
  c("log_pct",   "log_il8")
)

make_candidates <- function(score) {
  candidates <- list()
  
  # Reference
  candidates[["Reference"]] <- as.formula(paste("outcome ~", score))
  
  # Single biomarkers
  for (b in biomarkers) {
    candidates[[bio_label(b)]] <- as.formula(paste("outcome ~", score, "+", b))
  }
  
  # Pairs
  for (pair in biomarker_pairs) {
    nm <- paste(bio_label(pair), collapse = " + ")
    candidates[[nm]] <- as.formula(paste("outcome ~", score, "+", paste(pair, collapse = " + ")))
  }
  
  # All three
  candidates[["TREM-1 + PCT + IL-8"]] <-
    as.formula(paste("outcome ~", score, "+", paste(biomarkers, collapse = " + ")))
  
  candidates
}

# ============================================================
# 5) TRAINING: model selection using CV AUC only
#    IMPORTANT: use a fixed complete-case dataset per score so N is consistent
# ============================================================
train_results <- data.frame()
chosen_models <- list()

for (score in clinical_scores) {
  
  tmp_train <- df %>%
    select(outcome, all_of(score), all_of(biomarkers)) %>%
    na.omit()
  
  base_formula <- as.formula(paste("outcome ~", score))
  cand <- make_candidates(score)
  
  score_tbl <- lapply(names(cand), function(nm) {
    f <- cand[[nm]]
    res <- cv_auc_ci(f, tmp_train, K = 5)
    
    data.frame(
      Clinical_Score = toupper(score),
      Model = if (nm == "Reference") paste(toupper(score), "Reference") else paste(toupper(score), "+", nm),
      Biomarkers = if (nm == "Reference") "Reference" else nm,
      CV_AUC = res$auc_ci,
      CV_AUC_num = res$auc,   # <-- numeric for correct sorting
      N = nrow(tmp_train),
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  train_results <- bind_rows(train_results, score_tbl)
  
  # Select best NON-reference model (by numeric CV AUC)
  best_row <- score_tbl %>%
    filter(Biomarkers != "Reference") %>%
    arrange(desc(CV_AUC_num)) %>%
    dplyr::slice(1)
  
  chosen_models[[score]] <- list(
    base_formula  = base_formula,
    final_formula = cand[[best_row$Biomarkers]],
    label         = best_row$Biomarkers,
    trainN        = nrow(tmp_train)
  )
}

print(train_results)

# ============================================================
# 6) TEST EVALUATION HELPERS (NO FITTING ON TEST)
# ============================================================
auc_ci_from_preds <- function(y, p) {
  roc_obj <- roc(y, p, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  ci <- ci.auc(roc_obj)  # DeLong by default
  sprintf("%.3f (%.3f–%.3f)", auc_val, ci[1], ci[3])
}

delta_auc_from_preds <- function(y, p_ref, p_new) {
  roc_ref <- roc(y, p_ref, quiet = TRUE)
  roc_new <- roc(y, p_new, quiet = TRUE)
  round(as.numeric(auc(roc_new) - auc(roc_ref)), 3)
}

nri_continuous_ci_from_preds <- function(y, p_ref, p_new, R = 1000, seed = 123) {
  set.seed(seed)
  
  nri_point <- (
    mean(p_new[y == 1] > p_ref[y == 1]) - mean(p_new[y == 1] < p_ref[y == 1])
  ) + (
    mean(p_new[y == 0] < p_ref[y == 0]) - mean(p_new[y == 0] > p_ref[y == 0])
  )
  
  boot_data <- data.frame(y = y, p_ref = p_ref, p_new = p_new)
  
  boot_fn <- function(d, i) {
    dd <- d[i, ]
    yy <- dd$y
    (
      mean(dd$p_new[yy == 1] > dd$p_ref[yy == 1]) - mean(dd$p_new[yy == 1] < dd$p_ref[yy == 1])
    ) + (
      mean(dd$p_new[yy == 0] < dd$p_ref[yy == 0]) - mean(dd$p_new[yy == 0] > dd$p_ref[yy == 0])
    )
  }
  
  boot_res <- boot(boot_data, boot_fn, R = R)
  ci <- boot.ci(boot_res, type = "perc")$percent[4:5]
  
  list(
    estimate = round(nri_point, 2),
    lower    = round(ci[1], 2),
    upper    = round(ci[2], 2)
  )
}

# ============================================================
# 7) TESTING: Evaluate reference + final models
#    KEY: Fit on TRAIN, predict on TEST, compute metrics on TEST preds
# ============================================================
test_results_full <- data.frame()

for (score in clinical_scores) {
  
  # Use the same complete-case structure as training selection:
  tmp_train <- df %>%
    select(outcome, all_of(score), all_of(biomarkers)) %>%
    na.omit()
  
  tmp_test <- test_df %>%
    select(outcome, all_of(score), all_of(biomarkers)) %>%
    na.omit()
  
  base_formula  <- chosen_models[[score]]$base_formula
  final_formula <- chosen_models[[score]]$final_formula
  label         <- chosen_models[[score]]$label
  
  # Fit BOTH models on TRAIN ONLY
  fit_ref  <- glm(base_formula,  data = tmp_train, family = binomial)
  fit_new  <- glm(final_formula, data = tmp_train, family = binomial)
  
  # Predict on TEST ONLY
  p_ref <- predict(fit_ref, newdata = tmp_test, type = "response")
  p_new <- predict(fit_new, newdata = tmp_test, type = "response")
  y     <- tmp_test$outcome
  
  # Metrics computed on TEST preds
  ref_auc   <- auc_ci_from_preds(y, p_ref)
  final_auc <- auc_ci_from_preds(y, p_new)
  d_auc     <- delta_auc_from_preds(y, p_ref, p_new)
  
  nri_res <- nri_continuous_ci_from_preds(y, p_ref, p_new, R = 1000)
  nri_string <- sprintf("%.2f (%.2f–%.2f)", nri_res$estimate, nri_res$lower, nri_res$upper)
  
  test_results_full <- rbind(test_results_full, data.frame(
    Clinical_Score  = toupper(score),
    Reference_AUC   = ref_auc,
    Final_Model     = paste(toupper(score), "+", label),
    Final_AUC       = final_auc,
    Delta_AUC       = d_auc,
    Continuous_NRI  = nri_string,
    Train_N         = nrow(tmp_train),
    Test_N          = nrow(tmp_test),
    stringsAsFactors = FALSE
  ))
}

print(test_results_full)

# ============================================================
# 8) BRIDGE TABLE: Training CV AUC (selected) vs Test AUC (final)
# ============================================================
selected_df <- data.frame(
  Clinical_Score = toupper(names(chosen_models)),
  Selected_Biomarkers = sapply(chosen_models, function(x) x$label),
  stringsAsFactors = FALSE
)

train_table_export <- train_results %>%
  left_join(selected_df, by = "Clinical_Score") %>%
  mutate(Selected = ifelse(Biomarkers == Selected_Biomarkers, "Yes", "")) %>%
  select(Clinical_Score, Model, Biomarkers, CV_AUC, N, Selected) %>%
  arrange(Clinical_Score, desc(CV_AUC))

train_final_cv <- train_table_export %>%
  filter(Selected == "Yes") %>%
  select(Clinical_Score, Final_Model = Model, Training_CV_AUC = CV_AUC, Train_N = N)

bridge_table <- train_final_cv %>%
  left_join(
    test_results_full %>% select(Clinical_Score, Testing_AUC = Final_AUC, Delta_AUC, Continuous_NRI, Test_N),
    by = "Clinical_Score"
  ) %>%
  arrange(Clinical_Score)

print(bridge_table)

# ============================================================
# 9) EXPORT EXCEL (3 sheets)
# ============================================================
wb <- createWorkbook()

addWorksheet(wb, "TRAIN_all_combinations")
writeData(wb, "TRAIN_all_combinations", train_table_export)

addWorksheet(wb, "TEST_final_models")
writeData(wb, "TEST_final_models", test_results_full)

addWorksheet(wb, "FINAL_trainCV_vs_testAUC")
writeData(wb, "FINAL_trainCV_vs_testAUC", bridge_table)

saveWorkbook(wb, "Biomarker_Model_Tables_FIXED.xlsx", overwrite = TRUE)
message("Saved Excel: Biomarker_Model_Tables_FIXED.xlsx")

# ============================================================
# ROC FIGURE (LODS only): TRAIN vs TEST, reference vs final
# Fits on TRAIN, predicts on TRAIN + TEST (same coefficients)
# ============================================================

library(pROC)
library(dplyr)

score <- "lods_score"   # <-- LODS

# Complete-case data for this score + biomarkers (consistent with your pipeline)
tmp_train <- df %>%
  select(outcome, all_of(score), all_of(biomarkers)) %>%
  na.omit()

tmp_test <- test_df %>%
  select(outcome, all_of(score), all_of(biomarkers)) %>%
  na.omit()

# Pull selected model formulas from training CV selection
l_base  <- chosen_models[[score]]$base_formula
l_final <- chosen_models[[score]]$final_formula
l_label <- chosen_models[[score]]$label

# Fit on TRAIN only
fit_base  <- glm(l_base,  data = tmp_train, family = binomial)
fit_final <- glm(l_final, data = tmp_train, family = binomial)

# Predict on TRAIN + TEST
p_tr_base  <- predict(fit_base,  newdata = tmp_train, type = "response")
p_tr_final <- predict(fit_final, newdata = tmp_train, type = "response")
p_te_base  <- predict(fit_base,  newdata = tmp_test,  type = "response")
p_te_final <- predict(fit_final, newdata = tmp_test,  type = "response")

# ROC objects
roc_tr_base  <- roc(tmp_train$outcome, p_tr_base,  quiet = TRUE)
roc_tr_final <- roc(tmp_train$outcome, p_tr_final, quiet = TRUE)
roc_te_base  <- roc(tmp_test$outcome,  p_te_base,  quiet = TRUE)
roc_te_final <- roc(tmp_test$outcome,  p_te_final, quiet = TRUE)

# Save plot
png("ROC_LODS_training_vs_testing_FIXED.png", width = 1400, height = 1100, res = 150)

plot.roc(roc_tr_base, legacy.axes = TRUE, lwd = 3,
         main = "ROC Curves (LODS): Reference vs Biomarker Model\nTraining vs Testing (TRAIN-fit only)")

plot.roc(roc_tr_final, add = TRUE, lwd = 3, lty = 2)
plot.roc(roc_te_base,  add = TRUE, lwd = 3, lty = 3)
plot.roc(roc_te_final, add = TRUE, lwd = 3, lty = 4)

legend("bottomright",
       legend = c(
         paste0("Training: LODS (AUC=", round(as.numeric(auc(roc_tr_base)), 3), ")"),
         paste0("Training: LODS + ", l_label, " (AUC=", round(as.numeric(auc(roc_tr_final)), 3), ")"),
         paste0("Testing: LODS (AUC=", round(as.numeric(auc(roc_te_base)), 3), ")"),
         paste0("Testing: LODS + ", l_label, " (AUC=", round(as.numeric(auc(roc_te_final)), 3), ")")
       ),
       lty = c(1, 2, 3, 4),
       lwd = 3,
       bty = "n")

dev.off()

message("Saved ROC figure: ROC_LODS_training_vs_testing_FIXED.png")
