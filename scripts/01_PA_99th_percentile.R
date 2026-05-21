# =============================================================
# Author: Aleksander Solberg
# Date: 01.12.2025
# Purpose: Load, process and save physical activity variables for the IRONMAN physical activity study.
# Total duration of physical activity at moderate and vigorous intensity, as well as resistance training is derived, 
# Furthermore, adherence to resistance training and physical activity guidelines is calculated. 
# See Solberg et al. (2026) manuscript in preparation for details on processing.

# OBS! 
    # PA = physical activity, RT = resistance training

    # Standing and sitting questions are not included into physical activity estimates. 

    # valid_PA_data refers to having answering the physical activity oriented questions at a specific timepoint. 
    # Answering only standing or SB questions was not sufficient.  

    # The variable `valid_data` is defined as answering the physical activity oriented questions least once at
    # any timepoint, hence being included in subsequent analyses. 
# =============================================================
# ------------------------------------------------------------
# Set up environment
#--------------------------------------------------------------
#Load relevant packages
pacman::p_load(
  tidyverse, here
)

# Load proms data (we used relative file paths using the "here" package)
proms <- read.csv(here("data", "raw", "2025-02-24_proms.csv"))

# Extract PAQ answers and store PA variables in new data frame
PA <- proms %>%
  select(id = study_id, timepoint, starts_with("physactivity_")) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%  # Convert blanks to NA
  distinct()  # Remove fully duplicated rows

#--------------------------------------------------------------
# Data cleaning
#--------------------------------------------------------------
#Remove id and timepoint duplicates, keep most complete rows (manual inspection confirmed that this is appropriate)
PA <- PA %>%
  # 1) Count how many non‐missing physactivity_ fields each row has
  mutate(non_missing_count = rowSums(!is.na(select(., starts_with("physactivity_"))))) %>%
  
  # 2) For each id + timepoint, keep only the row with the highest non_missing_count
  group_by(id, timepoint) %>%
  slice_max(non_missing_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  # 3) Drop the helper column
  select(-non_missing_count)

#--------------------------------------------------------------
# Recode data to enable processing    
#--------------------------------------------------------------
# Define recoding functions (middle value for each categorical response is used)
recode_q2 <- function(x) {
  case_when(
    x == "zero" ~ 0,
    x == "1-4 minutes" ~ 2.5,
    x == "5-19 minutes" ~ 12,
    x == "20-59 minutes" ~ 39.5,
    x == "one hour" ~ 60,
    x == "1-1.5 hours" ~ 75,
    x == "2-3 hours" ~ 150,
    x == "4-6 hours" ~ 300,
    x == "7-10 hours" ~ 510,
    x == "11 or more hours" ~ 660,
    TRUE ~ NA_real_
  )
}

recode_q20_24 <- function(x) {
  case_when(
    x == "zero hours" ~ 0,
    x == "one hour" ~ 60,
    x == "2-5 hours" ~ 3.5 * 60,
    x == "6 to 10 hours" ~ 8 * 60,
    x == "11-20 hours" ~ 15.5 * 60,
    x == "21-40 hours" ~ 30.5 * 60,
    x == "41-60 hours" ~ 50.5 * 60,
    x == "61-90 hours" ~ 75.5 * 60,
    x == "over 90 hours" ~ 90 * 60,
    TRUE ~ NA_real_
  )
}

# Recode PA responses into numeric values in minutes
  PA_recoded <- PA %>%
    # physical activity and resistance training variables
    mutate(across(setdiff(names(.)[matches("^physactivity_[2-5]$|^physactivity_7$|^physactivity_9$|^physactivity_1[0-9]$")],
                          c("physactivity_6", "physactivity_8", "physactivity_10", "physactivity_12", "physactivity_14")),
                  ~ recode_q2(.))) %>%
    # sedentary time variables
    mutate(across(matches("^physactivity_20$|^physactivity_21$|^physactivity_22$|^physactivity_23$|^physactivity_24$"),
                  ~ recode_q20_24(.))) %>%
    mutate(across(matches("^physactivity_([2-5]|7|9|11|13|1[5-9]|2[0-4])"), ~ as.numeric(.)))

#--------------------------------------------------------------  
# Assess data completeness
#--------------------------------------------------------------
PA_recoded <- PA_recoded %>%
mutate(answered_PAQ = if_any(matches("physactivity_([2-9]|1[0-9])"), ~ !is.na(.)))

# Define valid / invalid data (answered the PAQ at any timepoint, at least once)
PA_recoded <- PA_recoded %>%
  mutate(valid_PA_data = answered_PAQ)

#Count n valid time points per participant
valid_timepoints_per_id <- PA_recoded %>%
  filter(valid_PA_data) %>%
  distinct(id, timepoint) %>%
  group_by(id) %>%
  summarise(n_valid_timepoints = n(), .groups = "drop") %>% 
  mutate(
    n_valid_timepoints = factor(n_valid_timepoints, levels = c("1", "2", "3"), ordered = TRUE)
  )

  #append n_valid_time points to PA variables
  PA_recoded <- PA_recoded %>%
    left_join(valid_timepoints_per_id, by = "id")

#--------------------------------------------------------------  
# Derive study variables
#-------------------------------------------------------------- 
#Calculate time spent in different activity categories
activity_duration <- PA_recoded %>%
  rowwise() %>%
  mutate(
    walking = physactivity_2,
    cycling = if (all(is.na(c_across(c(physactivity_5, physactivity_7, physactivity_9))))) {
      NA_real_
    } else {
      sum(c_across(c(physactivity_5, physactivity_7, physactivity_9)), na.rm = TRUE)
    },
    running = if (all(is.na(c_across(c(physactivity_3, physactivity_4))))) {
      NA_real_
    } else {
      sum(c_across(c(physactivity_3, physactivity_4)), na.rm = TRUE)
    },
    weight_training = if (all(is.na(c_across(c(physactivity_18, physactivity_19))))) {
      NA_real_
    } else {
      sum(c_across(c(physactivity_18, physactivity_19)), na.rm = TRUE)
    },
    raquet_sports = physactivity_11,
    swimming = physactivity_13,
    other_aerobic = physactivity_15,
    low_intensity_PA = physactivity_16,
    other_vig_PA = physactivity_17,
    standing_minutes_week = if (all(is.na(c_across(c(physactivity_20, physactivity_21))))) {
      NA_real_
    } else {
      sum(c_across(c(physactivity_20, physactivity_21)), na.rm = TRUE)
    },
    sitting_minutes_week = if (all(is.na(c_across(c(physactivity_22, physactivity_23, physactivity_24))))) {
      NA_real_
    } else {
      sum(c_across(c(physactivity_22, physactivity_23, physactivity_24)), na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Truncate activity type data at the 99th percentile for participants with valid data
  # Estimate 99th percentile
  activity_percentiles <- activity_duration %>%
    filter(valid_PA_data == TRUE) %>% 
    summarise(across(
      c(walking, cycling, running, weight_training, raquet_sports, swimming,
        other_aerobic, low_intensity_PA, other_vig_PA,
        standing_minutes_week, sitting_minutes_week),
      ~ quantile(.x, 0.99, na.rm = TRUE)
    ))
  
  # Show activity percentile
  print(activity_percentiles)
  

  # Apply truncation
  activity_duration <- activity_duration %>%
    mutate(across(
      c(walking, cycling, running, weight_training, raquet_sports, swimming,
        other_aerobic, low_intensity_PA, other_vig_PA,
        standing_minutes_week, sitting_minutes_week),
      ~ pmin(.x, activity_percentiles[[cur_column()]])
    ))


#Calculate time spent in different intensity categories (mod. and vig includes RT, aerobic_mod and _vig excludes RT)
  intensity_duration_raw <- PA_recoded %>%
    mutate(
      moderate_q2 = case_when(
        is.na(physactivity_2) ~ NA_real_,
        # Easy or normal pace → moderate
        physactivity_1 %in% c("easy, casual (less than 2 mph)",
                              "normal, average (2-2.9 mph)") ~ physactivity_2,
        # If pace is missing but minutes present → default to moderate
        is.na(physactivity_1) ~ physactivity_2,
        TRUE ~ 0
      ),
      vigorous_q2 = case_when(
        # If walking minutes are missing, keep as NA (avoid 0-from-missing)
        is.na(physactivity_2) ~ NA_real_,
        # Brisk or very brisk pace → vigorous
        physactivity_1 %in% c("brisk pace (3-3.9 mph)",
                              "very brisk/striding (4 mph or faster)") ~ physactivity_2,
        # If pace is missing → not vigorous
        is.na(physactivity_1) ~ 0,
        TRUE ~ 0
      ),
      vigorous_q3 = physactivity_3,
      vigorous_q4 = physactivity_4,
      moderate_q5 = ifelse(is.na(physactivity_6) | physactivity_6 != "high", physactivity_5, 0),
      vigorous_q5 = ifelse(physactivity_6 == "high", physactivity_5, 0),
      moderate_q7 = ifelse(is.na(physactivity_8) | physactivity_8 != "high", physactivity_7, 0),
      vigorous_q7 = ifelse(physactivity_8 == "high", physactivity_7, 0),
      moderate_q9 = ifelse(is.na(physactivity_10) | physactivity_10 != "high", physactivity_9, 0),
      vigorous_q9 = ifelse(physactivity_10 == "high", physactivity_9, 0),
      moderate_q11 = ifelse(is.na(physactivity_12) | physactivity_12 != "high", physactivity_11, 0),
      vigorous_q11 = ifelse(physactivity_12 == "high", physactivity_11, 0),
      moderate_q13 = ifelse(is.na(physactivity_14) | physactivity_14 != "high", physactivity_13, 0),
      vigorous_q13 = ifelse(physactivity_14 == "high", physactivity_13, 0),
      moderate_q15 = physactivity_15,
      moderate_q16 = physactivity_16,
      moderate_q18 = physactivity_18,
      moderate_q19 = physactivity_19,
      vigorous_q17 = physactivity_17
    ) %>%
    rowwise() %>%
    mutate(
      moderate_minutes_week_raw = sum(c_across(starts_with("moderate_")), na.rm = TRUE),
      vigorous_minutes_week_raw = sum(c_across(starts_with("vigorous_")), na.rm = TRUE),
      aerobic_moderate_minutes_week_raw = sum(c_across(c("moderate_q2", "moderate_q5", "moderate_q7", "moderate_q9", "moderate_q11",
                                                         "moderate_q13", "moderate_q15", "moderate_q16")), na.rm = TRUE)
    ) %>%
    ungroup()

# Truncate intensity data at the 99th percentile for participants with valid data
  # Calculate 99th percentile
  percentile_limits <- intensity_duration_raw %>%
    filter(valid_PA_data == TRUE) %>% 
    summarise(
      p99_moderate = quantile(moderate_minutes_week_raw, 0.99, na.rm = TRUE),
      p99_vigorous = quantile(vigorous_minutes_week_raw, 0.99, na.rm = TRUE),
      p99_aerobic_mod = quantile(aerobic_moderate_minutes_week_raw, 0.99, na.rm = TRUE)
    )

  # show intensity percentile
  print(percentile_limits)
  
  # Apply 99th percentile
  intensity_duration <- intensity_duration_raw %>%
    mutate(
      moderate_minutes_week = pmin(moderate_minutes_week_raw, percentile_limits$p99_moderate),
      vigorous_minutes_week = pmin(vigorous_minutes_week_raw, percentile_limits$p99_vigorous),
      aerobic_moderate_minutes_week = pmin(aerobic_moderate_minutes_week_raw, percentile_limits$p99_aerobic_mod),
      MVPA_minutes_week = moderate_minutes_week + vigorous_minutes_week,
      aerobic_MVPA_minutes_week = aerobic_moderate_minutes_week + vigorous_minutes_week
      )
  
#-------------------------------
# Extract variables of interest
#-------------------------------  
# Extract activity type variables
activity_vars <- activity_duration %>%
  left_join(PA_recoded %>% select(id, timepoint, walking_pace = physactivity_1), by = c("id", "timepoint")) %>%
  select(id, timepoint, walking_pace, walking, running, cycling, weight_training,
         raquet_sports, swimming, other_aerobic, low_intensity_PA,
         other_vig_PA, standing_minutes_week, sitting_minutes_week, answered_PAQ) %>% 
  mutate(walking_pace = factor(walking_pace))

# Extract intensity variables
intensity_vars <- intensity_duration %>%
  select(id, timepoint, moderate_minutes_week, vigorous_minutes_week, 
         MVPA_minutes_week, aerobic_moderate_minutes_week, aerobic_MVPA_minutes_week)

# Extract validity variables
validity_vars <- PA_recoded %>% 
  select(id, timepoint, valid_PA_data, n_valid_timepoints)

# Combine relevant variables into PA data set
PA_variables <- activity_vars %>%
  left_join(intensity_vars, by = c("id", "timepoint")) %>%
  left_join(validity_vars, by = c("id", "timepoint"))

#----------------------------------------
# Clean combined dataset
#----------------------------------------
# Handle wrongfully converted to zero NA's
replace_zeros_if_invalid <- function(df) {
  df[df$valid_PA_data == FALSE, ] <- df[df$valid_PA_data == FALSE, ] %>%
    mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))
  return(df)
}

PA_variables <- replace_zeros_if_invalid(PA_variables)

#----------------------------------------
# Calculate guideline adherence variables
#----------------------------------------
PA_variables <- PA_variables %>%
  mutate(
    aerobic_MPA_guidelines = if_else(
      !is.na(aerobic_moderate_minutes_week),
      aerobic_moderate_minutes_week >= 150,
      NA
    ),
    aerobic_VPA_guidelines = if_else(
      !is.na(vigorous_minutes_week),
      vigorous_minutes_week >= 75,
      NA
    ),
    aerobic_PA_guidelines = if_else(
      !is.na(aerobic_moderate_minutes_week) & !is.na(vigorous_minutes_week),
      aerobic_moderate_minutes_week + vigorous_minutes_week * 2 >= 150,
      NA
    ),
    RT_guidelines_30 = if_else(
      !is.na(weight_training),
      weight_training >= 30,
      NA
    ),
    joint_PA_guidelines = if_else(
      !is.na(aerobic_PA_guidelines) & !is.na(RT_guidelines_30),
      aerobic_PA_guidelines & RT_guidelines_30,
      NA
    ),
    any_RT = if_else(
      !is.na(weight_training),
      weight_training > 0,
      NA
    ),
    no_guidelines = if_else(
      !is.na(aerobic_PA_guidelines) & !is.na(RT_guidelines_30),
      !aerobic_PA_guidelines & !RT_guidelines_30,
      NA
    )
  )

#----------------------------------------
# Clean metadata (valid data indicators and n_valid_timepoints)
#----------------------------------------
#Ensure entire rows of valid id's are coded as TRUE and that n_valid_timepoints is repeated for all observations
PA_variables <- PA_variables %>%
  # Define valid_data per row (logical TRUE/FALSE)
  mutate(valid_data = if_else(valid_PA_data == TRUE, TRUE, FALSE, missing = FALSE)) %>%
  
  # For each participant, propagate valid_data and n_valid_timepoints
  group_by(id) %>%
  mutate(
    valid_data = any(valid_data),  # TRUE if any row valid
    
  # Fix for max() to handle all-NA cases in n_valid_timepoints
    n_valid_timepoints = max(as.numeric(as.character(n_valid_timepoints)), na.rm = TRUE)
  ) %>%

  # Handle case where all n_valid_timepoints are NA or -Inf
  mutate(
    n_valid_timepoints = if_else(is.infinite(n_valid_timepoints), NA_real_, n_valid_timepoints)
  ) %>%
  
  ungroup()
#----------------------------------------
#Save cleaned and processed data
#----------------------------------------
saveRDS(PA_variables, here("data", "processed", "PA_processed.rds"))

#---------------------------------
# Simple exploration of data set for quality assurance
#--------------------------------
explore_PA_variables <- PA_variables %>%
  filter(valid_data == TRUE) 

  str(explore_PA_variables)
  summary(explore_PA_variables)