# ------------------------------------------------------------
# Script: Off Study Information Extraction and Cleaning
# Author: Aleksander Solberg
# Date: 26.06.2025
# Purpose: Load and save relevant off study information  for the IRONMAN PA Study
# ------------------------------------------------------------

#Load relevant packages
  pacman::p_load(
    tidyverse, here
  )

# Load demographic variables from proms
  subject <- read.csv(here("data", "raw", "subject.csv"))

#Extract of study and exclusion information from subject file
  off_study_info <- subject %>% 
    select(id = subject, starts_with("offs"), enrolled, consent_date = cnstdate_int) %>% 
    mutate(
      offsdt_int = as.Date(offsdt_int, format = "%Y-%m-%d"),
      offstudyreason = factor(offstudyreason),
      consent_date = as.Date(consent_date, format = "%Y-%m-%d")
    )

# Manage time point duplicates with different responses (keep the most complete row per participant and timepoint)
  off_study_info <- off_study_info %>%
    group_by(id) %>%
    mutate(na_count = rowSums(is.na(across(everything())))) %>%
    arrange(na_count) %>%
    slice(1) %>%
    ungroup() %>%
    select(-na_count)

#Save relevant study data for merging
  saveRDS(off_study_info, here("data", "processed", "off_study_info.rds"))
  message("Off study information extracted and saved.")