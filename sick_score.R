library(dplyr)
library(pROC)
library(caret)

set.seed(123)

# ===========================
# 1. Read full dataset
# ===========================
full_df <- read.csv("finalized_dataset.csv", stringsAsFactors = FALSE)

exclude_ids <- c(196, 351, 668, 884, 917)

df <- full_df %>%
  filter(!record_id %in% exclude_ids)

# ===========================
# 2. Convert mortality to numeric
# ===========================
df <- df %>%
  mutate(
    outcome_numeric = ifelse(mort_inhosp == "Died", 1,
                             ifelse(mort_inhosp == "Survived", 0, NA))
  )

# Quick check
table(df$outcome_numeric, useNA = "ifany")

# ===========================
# 3. Map AVPU to numeric
# ===========================
df <- df %>%
  mutate(
    avpu_numeric = case_when(
      avpu_emd == "Alert" ~ 4,
      avpu_emd == "Verbal" ~ 3,
      avpu_emd == "Pain" ~ 2,
      avpu_emd == "Unresponsive" ~ 1,
      TRUE ~ NA_real_
    )
  )

# ===========================
# 4. Define individual SICK variables
# ===========================
df <- df %>%
  mutate(
    sick_temp = ifelse(!is.na(emd_temp) & (emd_temp > 38 | emd_temp < 36), 1, 0),
    
    sick_hr = ifelse(
      !is.na(age_calc) & !is.na(emd_hr) &
        ((age_calc < 1 & emd_hr > 160) | (age_calc >= 1 & emd_hr > 150)),
      1, 0
    ),
    
    sick_rr = ifelse(
      !is.na(age_calc) & !is.na(emd_rr) &
        ((age_calc < 1 & emd_rr > 60) | (age_calc >= 1 & emd_rr > 50)),
      1, 0
    ),
    
    sick_sbp = ifelse(
      !is.na(age_calc) & !is.na(emd_sbp) &
        ((age_calc < 1 & emd_sbp < 65) |
           (age_calc >= 1 & age_calc < 60 & emd_sbp < 75)),
      1, 0
    ),
    
    sick_spo2 = ifelse(!is.na(emd_sat) & emd_sat < 90, 1, 0),
    
    sick_cft = ifelse(!is.na(ss_cv___1) & ss_cv___1 >= 3, 1, 0),
    
    sick_avpu = ifelse(!is.na(avpu_numeric) & avpu_numeric < 4, 1, 0)
  )

# ===========================
# 5. Create age score (months)
# ===========================
df <- df %>%
  mutate(
    age_score = case_when(
      is.na(age_calc)     ~ NA_real_,
      age_calc >= 60      ~ 0.0,
      age_calc >= 12      ~ 0.3,
      age_calc >= 1       ~ 1.0,
      age_calc < 1        ~ 2.2
    )
  )

# ===========================
# 6. Create weighted SICK score
# ===========================
df <- df %>%
  mutate(
    sick_score =
      1.2 * sick_temp +
      0.2 * sick_hr +
      0.4 * sick_rr +
      1.2 * sick_sbp +
      1.4 * sick_spo2 +
      1.2 * sick_cft +
      2.0 * sick_avpu +
      age_score
  )

# ===========================
# 7. Create binary high-risk SICK variable using published cutoff 2.5
# ===========================
df <- df %>%
  mutate(
    sick_high_risk = ifelse(!is.na(sick_score) & sick_score >= 2.5, 1, 0)
  )

# ===========================
# 8. Quick checks
# ===========================
summary(df$sick_score)
table(df$sick_high_risk, useNA = "ifany")
head(df$sick_score)

# ===========================
# 9. ROC and AUC using continuous weighted SICK score
# ===========================
roc_df <- df %>%
  filter(!is.na(outcome_numeric), !is.na(sick_score))

roc_obj <- roc(roc_df$outcome_numeric, roc_df$sick_score)

plot(
  roc_obj,
  main = "ROC Curve for Weighted SICK Score",
  col = "black",
  lwd = 2
)

auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 3)))

# ===========================
# 10. Confusion matrix using cutoff 2.5
# ===========================
cm_df <- df %>%
  filter(!is.na(outcome_numeric), !is.na(sick_high_risk))

cm <- confusionMatrix(
  factor(cm_df$sick_high_risk, levels = c(0, 1)),
  factor(cm_df$outcome_numeric, levels = c(0, 1)),
  positive = "1"
)

print(cm)
df$sick_score
# ===========================
# 11. Save updated dataset
# ===========================
write.csv(df, "finalized_dataset.csv", row.names = FALSE)
