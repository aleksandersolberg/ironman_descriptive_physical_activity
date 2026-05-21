#--------------------------------------------------------------------------------
# Script: IM PA study - table formatting

# Author: Aleksander Solberg

# Purpose: Baseline descriptives by race
#--------------------------------------------------------------------------------
# Load packages
pacman::p_load(tidyverse, here, gtsummary, flextable, officer)

# Define base path
base_path <- here::here("output", "raw")

# Load data frames
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))

#Set default settings for flextable
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

#--------------------------------------------------------------------------------
# Table 1 - descriptive characteristics overall
#--------------------------------------------------------------------------------  
# Lag en baseline-tabell med én rad per ID
baseline_unique <- data_filtered %>%
  filter(timepoint == "Baseline") %>%
  distinct(id, .keep_all = TRUE)

# Overall-tabell med én rad per ID
table_1 <- baseline_unique %>%
  tbl_summary(
    include = c(
      age, BMI, country_region,
      race_simplified, marital_status_binary, 
      employment_ternary, education_binary,
      EORTC_QLQTOTAL, psa, ecog,
      disease_state, treatment_group_Baseline,
      time_on_study_months
    ),
    by = race_simplified,
    label = list(
      age ~ "Age (years)",
      BMI ~ "BMI",
      country_region ~ "Geographic Region",
      marital_status_binary ~ "Marital Status",
      employment_ternary ~ "Employment Status",
      education_binary ~ "Education Level",
      EORTC_QLQTOTAL ~ "EORTC QLQ-C30 Total Score",
      disease_state ~ "Disease State",
      psa ~ "PSA (ng/mL)",
      ecog ~ "ECOG Performance Status",
      treatment_group_Baseline ~ "First On-Study Treatment Regimen",
      time_on_study_months ~ "Time on Study (Months)"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "always",
    missing_text = "Missing (n)",
    digits = list(psa ~ 2, age ~ 1, EORTC_QLQTOTAL ~ 1)
  ) %>%
  bold_labels()

#--------------------------------------------------------------------------------
# Save tables to word format
#--------------------------------------------------------------------------------
table_1 %>%
  as_flex_table() %>%
  bold(part = "header") %>%
  set_caption("Table 1. Baseline Characteristics Overall and by Timepoint", 
              align_with_table = FALSE) %>%
  save_as_docx(path = here("output", "supplemental", "baseline_descriptives", "suppl_table_1_by_race.docx"))

table_1