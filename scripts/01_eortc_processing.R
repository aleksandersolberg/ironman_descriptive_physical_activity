# ------------------------------------------------------------
# Script: EORTC QLQ-C30 Scoring and Cleaning
# Author: Aleksander Solberg
# Date: 26.06.2025
# Purpose: Load, clean, score, and save EORTC QLQ-C30 data for the IRONMAN PA Study
# ------------------------------------------------------------

#Load relevant packages

  pacman::p_load(
    PROscorer, tidyverse, here
  )


# Load proms data
    proms <- read.csv(here("data", "raw", "2025-02-24_proms.csv"))

  # Extract eortc questions from proms data  
    eortc <- proms %>%
      select(
        id = study_id,
        timepoint,
        starts_with("eortc")
      ) %>%
      mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
      distinct()  # Remove fully duplicated rows

#Prepare dataframe to use qlq_c30 function from PROscorer package (data needs to be numeric)
  #define recoding values
    recode_vec <- c(
      "not at all" = 1,
      "a little" = 2,
      "quite a bit" = 3,
      "very much" = 4
    )
  
  #define variables to recode
    items <- paste0("eortc_", 1:28)
  
  #recode items
    eortc <- eortc  %>% 
      mutate(across(all_of(items), ~ recode_vec[.x]))
  
#Run scoring function from PROscorer package  
  eortc_scored <- qlq_c30(df = eortc, iprefix = "eortc_")
    message("Scoring EORTC QLQ-C30...")
  
  #rejoin scored dataset with id and timepoint variables
    eortc_scored <- bind_cols(eortc[c("id", "timepoint")], eortc_scored)

  # Rename scored variables to "EORTC_" to ease upcoming analyses
    eortc_scored <- eortc_scored %>%
      rename_with(~ paste0("EORTC_", .), -c(id, timepoint))
    
# Manage timepoint duplicates with different responses (keep the most complete row per participant and timepoint)
  eortc_scored <- eortc_scored %>%
    group_by(id, timepoint) %>%
    mutate(na_count = rowSums(is.na(across(everything())))) %>%
    arrange(na_count) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, timepoint, EORTC_QLQTOTAL)
  
#Save cleaned and processed data
  saveRDS(eortc_scored, here("data", "processed", "eortc_scored.rds"))
  message("EORTC data processed and saved.")