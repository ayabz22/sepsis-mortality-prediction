# ============================================================
# Table 1A (Clinical/Baseline) by Survival Status
# ============================================================
library(tidyverse)
library(gtsummary)
library(dplyr)
library(openxlsx)

# Load data 
df <- read.csv("finalized_dataset.csv", stringsAsFactors = FALSE)
exclude_ids <- c(196, 351, 668, 884, 917)

df <- df %>%
  filter(!record_id %in% exclude_ids)

# ------------------------------------------------------------
# 1) Create survival group (survived vs died)
# ------------------------------------------------------------
df <- df %>%
  mutate(
    survival_group = case_when(
      mort_inhosp == "Survived" ~ "Survivors",
      mort_inhosp == "Died"     ~ "Non-Survivors",
      TRUE ~ NA_character_
    ),
    survival_group = factor(survival_group, levels = c("Survivors", "Non-Survivors"))
  )

df_t1 <- df %>% filter(!is.na(survival_group))

n_surv  <- sum(df_t1$survival_group == "Survivors", na.rm = TRUE)
n_non   <- sum(df_t1$survival_group == "Non-Survivors", na.rm = TRUE)
n_total <- nrow(df_t1)

# ------------------------------------------------------------
# 2) Check vaccination variable and recode (0 = Yes, 1 = No)
# ------------------------------------------------------------
df_t1 <- df_t1 %>%
  mutate(
    immun_clean = case_when(
      immun == 0 ~ "Yes",
      immun == 1 ~ "No",
      TRUE ~ NA_character_
    ),
    immun_clean = factor(immun_clean, levels = c("Yes", "No"))
  )

vax_summary <- tibble(
  metric = c(
    "N total (non-missing survival_group)",
    "N immun_clean == Yes",
    "N immun_clean == No",
    "N immun_clean Missing",
    "% Yes among non-missing immun_clean"
  ),
  value = c(
    n_total,
    sum(df_t1$immun_clean == "Yes", na.rm = TRUE),
    sum(df_t1$immun_clean == "No",  na.rm = TRUE),
    sum(is.na(df_t1$immun_clean)),
    round(
      100 * sum(df_t1$immun_clean == "Yes", na.rm = TRUE) /
        sum(!is.na(df_t1$immun_clean)),
      1
    )
  )
)

# ------------------------------------------------------------
# 3) Create altered mental status variable (anything not "Alert")
# ------------------------------------------------------------
df_t1 <- df_t1 %>%
  mutate(
    ams = case_when(
      avpu_emd %in% c("Alert", "A", "ALERT") ~ "Alert",
      avpu_emd %in% c("Voice", "Verbal", "Pain", "Unresponsive", "V", "P", "U") ~ "Altered",
      TRUE ~ NA_character_
    ),
    ams = factor(ams, levels = c("Alert", "Altered"))
  )


# ------------------------------------------------------------
# 4) Variables for Table 1A (NO vital signs) + includes severity scores
#    - LODS as total score (median/IQR)
#    - SICK continuous (median/IQR)
#    - qSOFA as total score (median/IQR)
# ------------------------------------------------------------
table1a_vars <- c(
  "age_calc",
  "sex",
  "malnut_mod_sev.y",
  "immun_clean",
  "ams",
  "hgb_combo",
  "poct_rbg",
  "wbc_combo",
  "cx_blood_pos_any",
  "bact_pna",
  "hiv_pos",
  "poct_malaria_pos",
  "lods_score",
  "sick_score_cont",
  "qsofa",
  "los_total",
  "picu_admit_all",
  "pre_abx_bin"
)

# ------------------------------------------------------------
# 5) Build Table 1A using gtsummary
#    Treat severity scores as continuous (report median/IQR)
# ------------------------------------------------------------
tbl_1a <- df_t1 %>%
  select(all_of(c(table1a_vars, "survival_group"))) %>%
  tbl_summary(
    by = survival_group,
    type = list(
      lods_score ~ "continuous",
      qsofa ~ "continuous",
      sick_score_cont ~ "continuous"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ 1
    ),
    label = list(
      age_calc ~ "Age (months)",
      sex ~ "Male Sex",
      malnut_mod_sev.y ~ "Malnutrition",
      immun_clean ~ "Fully Vaccinated",
      ams ~ "Altered Mental Status (V/P/U)",
      hgb_combo ~ "Hemoglobin (g/dL)",
      poct_rbg ~ "Glucose (mmol/L)",
      wbc_combo ~ "White Blood Cell Count (/uL)",
      cx_blood_pos_any ~ "Bloodstream Infection",
      bact_pna ~ "Bacterial Pneumonia",
      hiv_pos ~ "HIV Positive",
      poct_malaria_pos ~ "Malaria Positive",
      lods_score ~ "LODS Score (total)",
      sick_score_cont ~ "SICK Score (continuous)",
      qsofa ~ "qSOFA Score (total)",
      los_total ~ "Length of Hospital Stay (days)",
      picu_admit_all ~ "Clinical Deterioration",
      pre_abx_bin ~ "Pre-Hospital Antibiotics"
    ),
    missing_text = "Missing"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  modify_header(
    label ~ "Baseline Characteristic",
    stat_1 ~ paste0("Survivors (n=", n_surv, ")"),
    stat_2 ~ paste0("Non-Survivors (n=", n_non, ")"),
    stat_0 ~ paste0("Total (n=", n_total, ")")
  ) %>%
  modify_spanning_header(
    c("stat_1", "stat_2", "stat_0") ~ "Survival Status"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ 
      "Continuous variables are reported as median (IQR); categorical variables as n (%). Percentages were calculated among non-missing observations. Missing counts are shown when applicable. IQR, interquartile range; LODS, Lambaréné Organ Dysfunction Score; qSOFA, quick Sequential Organ Failure Assessment score; SICK, physiologic sick score; AVPU, alert-verbal-pain-unresponsive; WBC, white blood cell count; HIV, Human Immunodeficiency Virus; g/dL, grams per deciliter; mmol/L, millimoles per liter; µL, microliter; bpm, beats per minute.",
    abbreviation = TRUE
  ) %>%
  modify_caption("Table 1A. Baseline Clinical Characteristics by Survival Status")

# ------------------------------------------------------------
# 6) Export to Excel
# ------------------------------------------------------------
tbl1a_df <- tbl_1a %>% as_tibble()

wb <- createWorkbook()

addWorksheet(wb, "Table1A")
writeData(wb, "Table1A", tbl1a_df)

addWorksheet(wb, "Vaccination_QC")
writeData(wb, "Vaccination_QC", vax_summary, startRow = 1, startCol = 1)

raw_counts_t1 <- as.data.frame(table(df_t1$immun, useNA = "ifany"))
names(raw_counts_t1) <- c("immun_value", "count")
raw_props_t1 <- raw_counts_t1 %>%
  mutate(percent = round(100 * count / sum(count), 1))

writeData(wb, "Vaccination_QC", raw_props_t1, startRow = nrow(vax_summary) + 4, startCol = 1)

saveWorkbook(wb, "Table1A_Baseline_Clinical.xlsx", overwrite = TRUE)

# ------------------------------------------------------------
# Check overall missingness 
# ------------------------------------------------------------

df_missing <- df_t1 %>% select(all_of(table1a_vars))
total_cells <- nrow(df_missing) * ncol(df_missing)
total_missing <- sum(is.na(df_missing))
# % missing
percent_missing <- round(100 * total_missing / total_cells, 2)
# ============================================================
# Table 1B (Biomarkers) by Survival Status
# - Back-transforms NATURAL LOG biomarkers to RAW units: raw = exp(log_x)
# - Reports median (IQR)
# - Exports to Excel 
# ============================================================

# Biomarkers currently stored as natural log
biomarkers_log <- c(
  "log_il10", "log_ang2", "log_il6", "log_il1ra", "log_il8",
  "log_tnfr1", "log_proc", "log_rage", "log_pai1", "log_icam1",
  "log_trem1", "log_crp", "log_fer", "log_pct",
  "log_lact", "log_hco3"
)

missing_log_vars <- setdiff(biomarkers_log, names(df_t1))
if (length(missing_log_vars) > 0) {
  stop("These log biomarker columns are missing from df_t1: ",
       paste(missing_log_vars, collapse = ", "))
}

df_t1 <- df_t1 %>%
  mutate(
    raw_il10  = exp(log_il10),
    raw_ang2  = exp(log_ang2),
    raw_il6   = exp(log_il6),
    raw_il1ra = exp(log_il1ra),
    raw_il8   = exp(log_il8),
    raw_tnfr1 = exp(log_tnfr1),
    raw_proc  = exp(log_proc),
    raw_rage  = exp(log_rage),
    raw_pai1  = exp(log_pai1),
    raw_icam1 = exp(log_icam1),
    raw_trem1 = exp(log_trem1),
    raw_crp   = exp(log_crp),
    raw_fer   = exp(log_fer),
    raw_pct   = exp(log_pct),
    raw_lact  = exp(log_lact),
    raw_hco3  = exp(log_hco3)
  )

biomarkers_raw <- c(
  "raw_il10", "raw_ang2", "raw_il6", "raw_il1ra", "raw_il8",
  "raw_tnfr1", "raw_proc", "raw_rage", "raw_pai1", "raw_icam1",
  "raw_trem1", "raw_crp", "raw_fer", "raw_pct",
  "raw_lact", "raw_hco3"
)

# Build Table 1B
tbl_1b <- df_t1 %>%
  select(all_of(c(biomarkers_raw, "survival_group"))) %>%
  tbl_summary(
    by = survival_group,
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
    digits = list(all_continuous() ~ 2),
    label = list(
      raw_il10  ~ "IL-10 (raw)",
      raw_ang2  ~ "Ang-2 (raw)",
      raw_il6   ~ "IL-6 (raw)",
      raw_il1ra ~ "IL-1RA (raw)",
      raw_il8   ~ "IL-8 (raw)",
      raw_tnfr1 ~ "TNFR1 (raw)",
      raw_proc  ~ "Protein C (raw)",
      raw_rage  ~ "RAGE (raw)",
      raw_pai1  ~ "PAI-1 (raw)",
      raw_icam1 ~ "ICAM-1 (raw)",
      raw_trem1 ~ "TREM-1 (raw)",
      raw_crp   ~ "C-reactive protein (raw)",
      raw_fer   ~ "Ferritin (raw)",
      raw_pct   ~ "Procalcitonin (raw)",
      raw_lact  ~ "Lactate (raw)",
      raw_hco3  ~ "HCO3 (raw)"
    ),
    missing_text = "Missing"
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
  modify_header(
    label ~ "Biomarker",
    stat_1 ~ paste0("Survivors (n=", n_surv, ")"),
    stat_2 ~ paste0("Non-Survivors (n=", n_non, ")"),
    stat_0 ~ paste0("Total (n=", n_total, ")")
  ) %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_0") ~ "Survival Status") %>%
  modify_footnote(
    all_stat_cols() ~
      "Biomarker values were back-transformed from natural log scale using exp(x). Continuous variables are reported as median (IQR).",
    abbreviation = TRUE
  ) %>%
  modify_caption("Table 1B. Biomarkers by Survival Status (raw values)")

# Export Table 1B to Excel
tbl1b_df <- tbl_1b %>% as_tibble()

if (!exists("wb")) wb <- createWorkbook()

if ("Table1B_Biomarkers" %in% names(wb)) removeWorksheet(wb, "Table1B_Biomarkers")
addWorksheet(wb, "Table1B_Biomarkers")
writeData(wb, "Table1B_Biomarkers", tbl1b_df)

saveWorkbook(wb, "Table1A_1B.xlsx", overwrite = TRUE)

