# ------------------------------------------------------------
# Script: Data merging IRONMAN PA study
# Author: Aleksander Solberg
# Date: 26.06.2025
# Purpose: Merge and save key study variables by ID and timepoint for the IRONMAN PA Study
# ------------------------------------------------------------
#Load relevant packages
pacman::p_load(
  tidyverse, here, lubridate
)
#----------------------------------
# Merge cleaned data
#----------------------------------
#Merge data files to master file
# Load data sets
subject <- read_csv(here("data", "raw", "subject.csv"))
demographics <- readRDS(here("data", "processed", "demographics.rds"))
eortc <- readRDS(here("data", "processed", "eortc_scored.rds"))
medical <- readRDS(here("data", "processed", "medical_cleaned.rds"))
off_study <- readRDS(here("data", "processed", "off_study_info.rds"))
pa_processed <- readRDS(here("data", "processed", "PA_processed.rds"))

# Merge on id and timepoints
all_ids <- bind_rows(
  demographics %>% select(id, timepoint),
  eortc %>% select(id, timepoint),
  medical %>% select(id, timepoint),
  pa_processed %>% select(id, timepoint)
) %>% distinct()

data <- all_ids %>%
  left_join(demographics, by = c("id", "timepoint")) %>%
  left_join(eortc, by = c("id", "timepoint")) %>%
  left_join(medical, by = c("id", "timepoint")) %>%
  left_join(pa_processed, by = c("id", "timepoint")) %>%
  left_join(off_study, by = "id")

# Add disease state info
#extract disease stage from cohort file
cohort <- read.csv(here("data", "raw", "cohort.csv")) %>%
  distinct()

disease_state <- cohort %>% 
  select(id = subject, disease_state = cohort) %>% 
  mutate(
    disease_state = trimws(disease_state), # Remove leading/trailing spaces
    disease_state = na_if(disease_state, ""), # Convert blank string to NA
    disease_state = factor(disease_state, levels = c("mHSPC", "CRPC")),
  )

data <- data  %>% 
  left_join(disease_state, by = "id")

#---------------------------
# Clean merged data
#----------------------------
#Correct and properly order timepoint variables
data$timepoint <- factor(
  data$timepoint,
  levels = c(
    "Baseline", "Month 3", "Month 6", "Month 9", "Month 12", "Month 15", "Month 18",
    "Month 21", "Month 24", "Month 27", "Month 30", "Month 33", "Month 36",
    "Month 39", "Month 42", "Month 45", "Month 48", "Month 51", "Month 54",
    "Month 57", "Month 60"
  ),
  ordered = TRUE
)

# Replace baseline values for participants with baseline timepoint to impute 
#mising baseline characteristics
baseline_unique <- data %>%
  filter(timepoint == "Baseline") %>%
  distinct(id)

missing_baseline_ids <- setdiff(unique(data$id), baseline_unique$id)

# Extract first available measurement
earliest_obs <- data %>%
  filter(id %in% missing_baseline_ids) %>%
  group_by(id) %>%
  arrange(timepoint) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(timepoint = factor("Baseline", levels = levels(data$timepoint), ordered = TRUE))

# Replace missing baseline values
data <- data %>%
  filter(!(id %in% missing_baseline_ids & timepoint == "Baseline")) %>%
  bind_rows(earliest_obs)

#Manage duplicates
data <- data %>%
  distinct(id, timepoint, .keep_all = TRUE)
#----------------------------------------------
#Prepare merged data set for exploratory analyses       
#-----------------------------------------------
#Generate binary variables
data <- data %>%
  mutate(
    marital_status_binary = factor(case_when(
      tolower(as.character(marital_status)) == "married" ~ "Married",
      !is.na(marital_status) ~ "Not Married",
      TRUE ~ NA_character_
    ), levels = c("Married", "Not Married")),
    
    living_arrangement_binary = factor(case_when(
      tolower(as.character(living_arrangement)) %in% tolower(c(
        "With partner", 
        "With roommates or friends", 
        "With family (not partner)"
      )) ~ "Lives Together",
      !is.na(living_arrangement) ~ "Lives Alone or Institutional",
      TRUE ~ NA_character_
    ), levels = c("Lives Together", "Lives Alone or Institutional")),
    
    education_binary = factor(case_when(
      education %in% c("Higher Education") ~ "Higher Education",
      !is.na(education) ~ "Less than higher education",
      TRUE ~ NA_character_
    ), levels = c("Higher Education", "Less than higher education")),
    
    employment_ternary = factor(case_when(
      tolower(as.character(employment)) %in% c("full time work", "part-time work") ~ "Working",
      tolower(as.character(employment)) %in% c("retired") ~ "Retired",
      !is.na(employment) ~ "Unemployed or Other",
      TRUE ~ NA_character_
    ), levels = c("Working", "Unemployed or Other", "Retired")),
    
    ecog_binary = factor(
      case_when(
        as.numeric(as.character(ecog)) == 0 ~ "0",
        as.numeric(as.character(ecog)) == 1 ~ "1",
        as.numeric(as.character(ecog)) >= 2 ~ "≥2",
        TRUE ~ NA_character_
      ),
      levels = c("0", "1", "≥2")
    ),
    gleason_trinary = case_when(
      gleason_factor %in% c("8", "9", "10") ~ "8-10",
      gleason_factor %in% c("<=6") ~ "<=6",
      gleason_factor %in% c("7") ~ "7",
      is.na(gleason_factor) ~ NA_character_,
      TRUE ~ "Other"
    ) %>%
      factor(levels = c("<=6", "7", "8-10"))
  )

#Merge marital status and living arrangement to married or living with partner as category
data <- data %>%
  mutate(
    marital_status_binary = case_when(
      marital_status_binary == "Married" ~ "Married or Living With Partner",
      living_arrangement_binary == "Lives Together" ~ "Married or Living With Partner",
      TRUE ~ marital_status_binary
    ),
    marital_status_binary = factor(
      marital_status_binary,
      levels = c("Married or Living With Partner", "Not Married")
    )
  )
   
# Create subgroups of continuous variables
data <- data %>% 
  mutate(
    BMI_subgroup = cut(BMI, breaks = c(-Inf, 25, 30, Inf),
                       labels = c("Normal Weight", "Overweight", "Obese")),
    age_subgroup = cut(age, breaks = c(-Inf, 65, 75, Inf),
                       labels = c("<65 years", "65 to 74 years", ">75 years"))
  )

#Specify reference levels
data <- data %>% 
  mutate(
    marital_status_binary = relevel(marital_status_binary, ref = "Married or Living With Partner"),
    living_arrangement_binary = relevel(living_arrangement_binary, ref = "Lives Together"),
    education_binary = relevel(education_binary, ref = "Higher Education"),
    ecog_binary = relevel(ecog_binary, ref = "0"),
    gleason_trinary = relevel(gleason_trinary, ref = "<=6"),
    race_simplified = relevel(race_simplified, ref = "White"),
    country_income_group = relevel(country_income_group, ref = "High Income"),
    country_region = relevel(country_region, ref = "Europe"),
    treatment_group = relevel(treatment_group_Baseline, ref = "ADT + ARPI"),
    disease_state = relevel(disease_state, ref = "mHSPC"),
    age_subgroup = relevel(age_subgroup, ref = "<65 years"),
    BMI_subgroup = relevel (BMI_subgroup, ref = "Normal Weight"),
    employment_ternary = relevel(employment_ternary, ref = "Working"),
    n_valid_timepoints = factor(n_valid_timepoints)
  )

# convert site to factor
data <- data %>% 
  mutate(site = factor(site))

#Calculate time on study using last registred timepoint per participant
#Filter out duplicate timepoints
data_id <- data %>%
  distinct(id, consent_date)

#Convert timepoint to a numeric estimate representing months
data <- data %>%
  mutate(
    timepoint_month = case_when(
      timepoint == "Baseline" ~ 0,
      str_detect(timepoint, "Month") ~ as.numeric(str_remove(timepoint, "Month ")),
      TRUE ~ NA_real_
    )
  )

#Extract last registred timepoint per participant
last_timepoint <- data %>%
  group_by(id) %>%
  summarise(
    last_month = suppressWarnings(max(timepoint_month, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    last_month = ifelse(is.infinite(last_month), NA_real_, last_month)
  )

#Generate time on study in months 
data_id <- data_id %>%
  left_join(last_timepoint, by = "id") %>%
  mutate(
    time_on_study_months = pmin(last_month, 60)
  )

#Merge variable back to main dataset
data <- data %>%
  left_join(
    data_id %>% select(id, time_on_study_months),
    by = "id"
  )

# Assess n participants reaching 6 month follow-up by 
# 01.01.2028 (date of inclusion of PA questionnaire)

data <- data %>% 
  mutate(
    m6_date = consent_date %m+% months(6)
  ) 

#---------------------------------------------
# Create analysis datasets by filtering on valid data status
#---------------------------------------------
#Define valid vs invalid study data, and define missing PA data
#Specify timepoints of interest
target_timepoints <- c("Baseline", "Month 6", "Month 18", "Month 30") 

#Specify valid ids
valid_ids <- data %>%
  group_by(id) %>%
  summarise(has_valid = any(valid_data == TRUE), .groups = "drop") %>% 
  filter(has_valid) %>%
  pull(id)

data_filtered <- data %>%
  filter(id %in% valid_ids, timepoint %in% target_timepoints) %>%
  filter(valid_data == TRUE) %>% 
  droplevels()

#Remove wrongfully included ids (participants without data)
# List of IDs to filter out
exclude_ids <- c("170-37-035", "170-43-008", "170-43-025", "170-53-053", "170-56-090")

# Filter out these participants
data_filtered <- data_filtered %>%
  filter(!id %in% exclude_ids)

# Data frame for invalid ids
# List of IDs to ensure are included in invalid data
exclude_ids <- c("170-37-035", "170-43-008", "170-43-025", "170-53-053", "170-56-090")

# Filter for invalid ids, including the specified exclude_ids
invalid_data <- data %>%
  filter((!id %in% valid_ids & timepoint %in% target_timepoints) | id %in% exclude_ids) %>%
  droplevels()

# Impute missing baseline data for participants missing baseline using first available measurement
data_filtered_2 <- data_filtered %>%
  filter(timepoint == "Baseline")

# Identify missing IDs
missing_ids <- setdiff(unique(data_filtered$id), unique(data_filtered_2$id))

# Extract first available record for each missing ID
first_records <- data_filtered %>%
  filter(id %in% missing_ids) %>%
  arrange(id, timepoint) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(timepoint = "Baseline",
         imputed_baseline = TRUE)  # Flag for transparency

# Append imputed rows to original dataset
data_filtered <- bind_rows(data_filtered, first_records)

# Check result
table(data_filtered$timepoint)

# Check for duplicates
anyDuplicated(data_filtered %>% select(id, timepoint))

#Ensure timepoints is a factor
data_filtered <- data_filtered %>% 
  mutate(
    timepoint = factor(timepoint, ordered = TRUE)
  )

#Generate df for varying amounts of complete cases (1, 2 or 3 valid PAQ)
data_filtered_one_valid <- data_filtered %>%
  filter(n_valid_timepoints == 1) %>%
  droplevels()

data_filtered_two_valid <- data %>%
  filter(n_valid_timepoints == 2) %>%
  droplevels()

data_filtered_three_valid <- data %>%
  filter(n_valid_timepoints == 3) %>%
  droplevels()

#Save each data frame
saveRDS(data, here("data", "processed", "data.rds"))
saveRDS(data_filtered, here("data", "processed", "data_filtered.rds"))
saveRDS(invalid_data, here("data", "processed", "invalid_data.rds"))
saveRDS(data_filtered_one_valid, here("data", "processed", "data_filtered_one_valid.rds"))
saveRDS(data_filtered_two_valid, here("data", "processed", "data_filtered_two_valid.rds"))
saveRDS(data_filtered_three_valid, here("data", "processed", "data_filtered_three_valid.rds"))



# Create a participant-level summary of valid timepoints
valid_timepoints_per_id <- data %>%
  filter(valid_PA_data) %>%
  distinct(id, timepoint) %>%
  group_by(id) %>%
  summarise(n_valid_timepoints = n(), .groups = "drop")

# Count number of participants with at least 1, 2, 3 valid timepoints
n_valid_timepoints_summary <- tibble(
  at_least_1 = sum(valid_timepoints_per_id$n_valid_timepoints == 1),
  at_least_2 = sum(valid_timepoints_per_id$n_valid_timepoints == 2),
  at_least_3 = sum(valid_timepoints_per_id$n_valid_timepoints == 3)
)

# check influence of version 3 protocol (conclusion = no influence, all participants reaching m6 were after 2018)
data_filtered <- data_filtered %>% 
  arrange(m6_date) %>% 
  relocate(m6_date)

data <- data %>% 
  arrange(m6_date) %>% 
  relocate(m6_date, consent_date) %>% 
  distinct(id, .keep_all = TRUE)