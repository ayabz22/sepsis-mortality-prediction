# Load required packages
library(randomForest)
library(xgboost)
library(caret)
library(dplyr)
library(ggplot2)
library(purrr)

# -------------------------------
# Biomarkers and clinical scores
# -------------------------------
biomarkers <- c(
  "log_il10", "log_ang2", "log_il6", "log_il1ra", "log_il8",
  "log_tnfr1", "log_proc", "log_rage", "log_pai1", "log_icam1",
  "log_trem1", "log_crp", "log_fer", "log_pct",
  "log_lact", "log_hco3"
)

clinical_scores <- c("lods_score", "qsofa", "sick_score")


# Prepare dataset (remove missing values in biomarkers + outcome)
df$mort_inhosp
df <- df %>%
  mutate(mort_inhosp = factor(mort_inhosp, levels = c("Survived", "Died")))
df_clean <- df %>%
  select(all_of(biomarkers), mort_inhosp) %>%
  filter(complete.cases(.))
table(df_clean$mort_inhosp)


# Random Forest feature importance
set.seed(123)
rf_model <- randomForest(
  mort_inhosp ~ ., 
  data = df_clean,
  mtry = floor(sqrt(length(biomarkers))),
  ntree = 1000,
  maxnodes = 10,
  importance = TRUE
)


# Gini importance
rf_importance <- importance(rf_model, type = 2)
rf_ranked <- data.frame(
  Biomarker = rownames(rf_importance),
  GiniImportance = rf_importance[, "MeanDecreaseGini"]
) %>% arrange(desc(GiniImportance))


# XGBoost feature importance
x <- as.matrix(df_clean[, biomarkers])
y <- df_clean$mort_inhosp

# Train control
train_control <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# XGBoost grid
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(2,3,4),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Train XGBoost
xgb_model <- train(
  x = x,
  y = y,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "ROC"
)

# Get feature importance
xgb_imp <- varImp(xgb_model)$importance
xgb_ranked <- data.frame(
  Biomarker = rownames(xgb_imp),
  XGBImportance = xgb_imp$Overall
) %>% arrange(desc(XGBImportance))


# Plot feature importance
# Random Forest
ggplot(rf_ranked, aes(x = reorder(Biomarker, GiniImportance), y = GiniImportance)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(title = "Random Forest Biomarker Importance", x = "Biomarker", y = "Gini Importance")

# XGBoost
ggplot(xgb_ranked, aes(x = reorder(Biomarker, XGBImportance), y = XGBImportance)) +
  geom_col(fill = "#EFC000FF") +
  coord_flip() +
  labs(title = "XGBoost Biomarker Importance", x = "Biomarker", y = "Importance Score")

# Save Random Forest plot
rf_plot <- ggplot(rf_ranked, aes(x = reorder(Biomarker, GiniImportance),
                                 y = GiniImportance)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(
    title = "Random Forest Biomarker Importance",
    x = "Biomarker",
    y = "Gini Importance"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "Figure_RF_importance.png",
  plot = rf_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Save XGBoost plot
xgb_plot <- ggplot(xgb_ranked, aes(x = reorder(Biomarker, XGBImportance),
                                   y = XGBImportance)) +
  geom_col(fill = "#EFC000FF") +
  coord_flip() +
  labs(
    title = "XGBoost Biomarker Importance",
    x = "Biomarker",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "Figure_XGB_importance.png",
  plot = xgb_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Export figures to Word
library(officer)

doc <- read_docx() %>%
  body_add_par("Figure 1. Random Forest biomarker importance based on mean decrease in Gini index.",
               style = "Normal") %>%
  body_add_img(src = "Figure_RF_importance.png",
               width = 6, height = 4) %>%
  body_add_par("") %>%
  body_add_par("Figure 2. XGBoost biomarker importance based on model-derived importance scores.",
               style = "Normal") %>%
  body_add_img(src = "Figure_XGB_importance.png",
               width = 6, height = 4)

print(doc, target = "Feature_Importance_Figures.docx")

