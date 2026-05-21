#--------------------------------------------------------------------------------
# Script: IM PA study - supplement missing data analysis
# Author: Aleksander Solberg
# Purpose: Compare baseline characteristics between valid and invalid PA data
#--------------------------------------------------------------------------------

# Load relevant packages
pacman::p_load(tidyverse, gtsummary, flextable, here)

# Load data
data <- readRDS(here("data", "processed", "data.rds"))
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))
invalid_data <- readRDS(here("data", "processed", "invalid_data.rds"))
data_filtered_one_valid <- readRDS(here("data", "processed", "data_filtered_one_valid.rds"))
data_filtered_two_valid <- readRDS(here("data", "processed", "data_filtered_two_valid.rds"))
data_filtered_three_valid <- readRDS(here("data", "processed", "data_filtered_three_valid.rds"))

# Set default settings for flextable
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

# Define function for extracting baseline variables in Table 1 format
descriptives_function <- function(data, group_label = "Group") {
  data %>%
    filter(timepoint == "Baseline") %>%
    distinct(id, .keep_all = TRUE) %>%
    select(
      age,
      BMI,
      country_region,
      race_simplified,
      marital_status_binary,
      employment_ternary,
      education_binary,
      EORTC_QLQTOTAL,
      psa,
      ecog,
      disease_state,
      treatment_group_Baseline,
      time_on_study_months
    ) %>%
    mutate(group = group_label)
}

# Generate baseline descriptives for valid and invalid data
desc_data_baseline_valid <- descriptives_function(data_filtered, group_label = "Valid")
desc_data_baseline_invalid <- descriptives_function(invalid_data, group_label = "Invalid")

# Combine datasets
combined_desc_data <- bind_rows(desc_data_baseline_valid, desc_data_baseline_invalid)

# Create summary table
table_valid_vs_invalid <- combined_desc_data %>%
  tbl_summary(
    by = group,
    label = list(
      age ~ "Age (years)",
      BMI ~ "BMI",
      country_region ~ "Geographic Region",
      race_simplified ~ "Race",
      marital_status_binary ~ "Marital Status",
      employment_ternary ~ "Employment Status",
      education_binary ~ "Education Level",
      EORTC_QLQTOTAL ~ "EORTC QLQ-C30 Total Score",
      psa ~ "PSA (ng/mL)",
      ecog ~ "ECOG Performance Status",
      disease_state ~ "Disease State",
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

# Export to Word
table_valid_vs_invalid %>%
  as_flex_table() %>%
  set_caption("Supplementary Table: Baseline Characteristics by Validity of PA Data") %>%
  save_as_docx(path = here("output", "supplemental", "baseline_descriptives", "Supp_Table_S1.docx"))

# Function to standardize factor levels
standardize_factors <- function(df) {
  df %>%
    mutate(
      ecog = factor(ecog, levels = c("0", "1", "2", "3", "4")),
      country_region = as.factor(country_region),
      race_simplified = as.factor(race_simplified),
      marital_status_binary = as.factor(marital_status_binary),
      employment_ternary = as.factor(employment_ternary),
      education_binary = as.factor(education_binary),
      disease_state = as.factor(disease_state),
      treatment_group_Baseline = as.factor(treatment_group_Baseline)
    )
}

# Generate descriptives for each subset (one, two, and three valid PA measurements)
desc_data_one_valid <- descriptives_function(data_filtered_one_valid, group_label = "One valid measurement") %>% standardize_factors()
desc_data_two_valid <- descriptives_function(data_filtered_two_valid, group_label = "Two valid measurements") %>% standardize_factors()
desc_data_three_valid <- descriptives_function(data_filtered_three_valid, group_label = "Three valid measurements") %>% standardize_factors()

# Combine datasets for comparison by number of valid measurements
combined_desc_data_per_valid_t <- bind_rows(
  desc_data_one_valid,
  desc_data_two_valid,
  desc_data_three_valid
) %>%
  mutate(group = factor(group, levels = c(
    "One valid measurement",
    "Two valid measurements",
    "Three valid measurements"
  )))

# Create summary table comparing groups by number of valid measurements
valid_n_comp_table <- combined_desc_data_per_valid_t %>%
  tbl_summary(
    by = group,
    label = list(
      age ~ "Age (years)",
      BMI ~ "BMI",
      country_region ~ "Geographic Region",
      race_simplified ~ "Race",
      marital_status_binary ~ "Marital Status",
      employment_ternary ~ "Employment Status",
      education_binary ~ "Education Level",
      EORTC_QLQTOTAL ~ "EORTC QLQ-C30 Total Score",
      psa ~ "PSA (ng/mL)",
      ecog ~ "ECOG Performance Status",
      disease_state ~ "Disease State",
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
  bold_labels() %>%
  modify_caption("Supplementary Table: Baseline Characteristics by Number of Valid PA Measurements")

# Export to Word
valid_n_comp_table %>%
  as_flex_table() %>%
  save_as_docx(path = here("output", "supplemental", "baseline_descriptives", "Supp_Table_S2.docx"))
