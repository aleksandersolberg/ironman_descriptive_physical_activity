# ------------------------------------------------------------
# Script: Medical Data Extraction
# Author: Aleksander Solberg
# Date: 26.06.2025
# Purpose: Load, extract, clean and transform relevant medical data for IRONMAN PA study
# ------------------------------------------------------------
#Load relevant packages
pacman::p_load(
  tidyverse, here
)

#extract relevant medical data from subject.csv file
subject <- read.csv(here("data", "raw", "subject.csv")) %>% 
  distinct()

medical <- subject %>% 
  select(
    id = subject, BL_met_status = is_metastatic_baseline, gleason_factor)

#PSA values
#extract PSA value from psa.csv file
psa <- read.csv(here("data", "raw", "psa.csv")) %>% 
  distinct(subject, instance_name, .keep_all = TRUE)

psa_val <- psa %>% 
  select(id = subject, timepoint = instance_name, psa, hxpsaunit) %>% 
  mutate(psa = as.numeric(psa))

#PSA data cleaning
#Define function for converting to ng/mL     
convert_psa_to_ng_ml <- function(psa, unit) {
  unit <- tolower(trimws(unit))
  case_when(
    unit == "ng/ml" ~ psa,
    unit == "ug/l"  ~ psa,        # 1 ug/L = 1 ng/mL
    unit == "ug/ml" ~ psa * 1000, # 1 ug/mL = 1000 ng/mL
    TRUE ~ NA_real_               # everything else = missing
  )
}

# Calculate the 99th percentile threshold dynamically
psa_threshold <- quantile(psa_val$psa, 0.99, na.rm = TRUE)

# Apply conversion function and remove extreme PSA values
psa_val <- psa_val %>%
  mutate(
    psa = convert_psa_to_ng_ml(psa, hxpsaunit),  # Convert PSA values to ng/mL
    hxpsaunit = "ng/mL",                         # Standardize unit to ng/mL
    
    # Set PSA values greater than 99th percentile
    psa = case_when(
      psa > psa_threshold ~ NA_real_,  # Remove extreme values > 99th percentile
      TRUE ~ psa                      # Keep valid PSA values
    )
  )

#Append to main date frame
medical <- medical %>% 
  full_join(psa_val, by = "id")

#Load ecog.csv file
ecog <- read.csv(here("data", "raw", "vs.csv")) %>%
  distinct()

#Extract relevant ecog data and remove duplicate rows
ecog_status <- ecog %>%
  select(id = subject, timepoint = instance_name, ecog) %>%
  group_by(id, timepoint) %>%
  slice(1) %>% 
  ungroup()

#Assess for errors (values outside of 0 - 5)    
ecog_values <- unique(ecog_status$ecog)  
print(ecog_values) # No errors

#Append to main data frame
medical <- medical %>% 
  full_join(ecog_status, by = c("id", "timepoint"))

# ------------------------------------------------------------
# Clean treatment data
# ------------------------------------------------------------
ca_cm <- read.csv(here("data", "raw", "ca_cm.csv")) %>%
  distinct()

treatment <- ca_cm %>% 
  select(
    id = subject,
    treatment,
    exstdat,
    exendat,
    exongoing, 
    exdose,
    exunit
  ) %>%
  full_join(subject %>% select(id = subject, cnstdate_int), by = "id") %>%
  mutate(
    exstdat = ymd(exstdat),
    exendat = ymd(exendat),
    cnstdate_int = ymd(cnstdate_int),
    exunit = factor(exunit),
    exongoing = as.logical(exongoing),
    treatment_clean = treatment %>%
      tolower() %>%
      str_replace_all("\\(.*?\\)", "") %>%
      str_squish()
  )

# ------------------------------------------------------------
# Define treatment categories
# ------------------------------------------------------------

adt_terms <- c(
  "androgen deprivation therapy", "adt", "degarelix", "firmagon", "zoladex", "goserelin",
  "leuprolide", "lupron", "eligard", "decapeptyl", "triptorelin", "relugolix", "trelstar", 
  "orgovy", "suprefact", "vantas", "buserelin", "histrelin", "lhrh", "gnrh", "leuprorelin",
  "prostap", "orgovyx", "estradiol", "diethylstilbestrol"
)

aa_terms <- c(
  "bicalutamide", "casodex", "cyproterone", "androcure", "androcur", "flutamide", 
  "nilutamide", "anandron"
)

arpi_terms <- c(
  "abiraterone", "enzalutamide", "apalutamide", "darolutamide", 
  "xtandi", "erleada", "nubeqa", "zytiga"
)

chemo_terms <- c(
  "docetaxel", "cabazitaxel", "taxotere", 
  "cisplatin", "paclitaxel", "gemcitabine", 
  "cyclophosphamide", "fluorouracil"
)

# ------------------------------------------------------------
# Classify treatments
# ------------------------------------------------------------

treatment_classified <- treatment %>%
  mutate(
    has_adt = str_detect(treatment_clean, regex(paste(adt_terms, collapse = "|"), ignore_case = TRUE)),
    has_aa = str_detect(treatment_clean, regex(paste(aa_terms, collapse = "|"), ignore_case = TRUE)),
    has_chemo = str_detect(treatment_clean, regex(paste(chemo_terms, collapse = "|"), ignore_case = TRUE)),
    has_arpi = str_detect(treatment_clean, regex(paste(arpi_terms, collapse = "|"), ignore_case = TRUE)),
  )

# ------------------------------------------------------------
# Define helper function: is treatment active at timepoint?
# ------------------------------------------------------------
# Check if treatment is active within ±3 months of timepoint
is_active <- function(start, end, ongoing, tp_date) {
  window_start <- tp_date %m-% months(3)
  window_end   <- tp_date %m+% months(3)
  
  # Treat NA end dates with ongoing==TRUE as "still active"
  end_filled <- ifelse(is.na(end) & ongoing, as.Date("2100-01-01"), end)
  
  # Active if treatment interval overlaps the [window_start, window_end] interval
  (!is.na(start) & start <= window_end & (is.na(end_filled) | end_filled >= window_start))
}
# ------------------------------------------------------------
# Expand to timepoints (baseline, 6, 18, 30 months)
# ------------------------------------------------------------
timepoints <- tibble(
  tp = c("Baseline", "Month 6", "Month 18", "Month 30"),
  offset_months = c(0, 6, 18, 30)
)
# Create full grid of all participants × all timepoints
id_timepoints <- subject %>%
  select(id = subject, cnstdate_int) %>%
  mutate(cnstdate_int = ymd(cnstdate_int)) %>%
  crossing(timepoints) %>%
  mutate(tp_date = cnstdate_int %m+% months(offset_months))

# Attach treatment data
treatment_timepoint <- id_timepoints %>%
  full_join(treatment_classified, by = "id") %>%
  mutate(active = is_active(exstdat, exendat, exongoing, tp_date)) %>%
  group_by(id, tp) %>%
  summarise(
    has_adt   = any(has_adt   & active, na.rm = TRUE),
    has_aa    = any(has_aa    & active, na.rm = TRUE),
    has_arpi  = any(has_arpi  & active, na.rm = TRUE),
    has_chemo = any(has_chemo & active, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inferred_adt = has_adt | has_aa | has_arpi | has_chemo,
    treatment_group = case_when(
      inferred_adt & has_arpi & has_chemo ~ "ADT + ARPI + chemo",
      inferred_adt & has_arpi ~ "ADT + ARPI",
      inferred_adt & !has_arpi & has_chemo ~ "ADT + chemo",
      inferred_adt & !has_arpi & !has_chemo ~ "ADT monotherapy"
    )
  )
# ------------------------------------------------------------
# Output overview
# ------------------------------------------------------------
# Counts per timepoint
treatment_timepoint %>%
  count(tp, treatment_group) %>%
  arrange(tp, desc(n)) %>%
  print(n = 50)

treatment_timepoint

# Counts per timepoint
treatment_timepoint %>%
  count(tp, treatment_group) %>%
  arrange(tp, desc(n)) %>%
  print(n = 50)

treatment_wide <- treatment_timepoint %>%
  select(id, timepoint = tp, treatment_group) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = treatment_group,
    names_prefix = "treatment_group_"
  ) %>% 
  mutate(
    treatment_group_Baseline = factor(treatment_group_Baseline),
    `treatment_group_Month 18` = factor(`treatment_group_Month 18`),
    `treatment_group_Month 30` = factor(`treatment_group_Month 30`),
    `treatment_group_Month 6` = factor(`treatment_group_Month 6`) 
  )

# Join treatment group info to  medical dataset
medical <- medical %>%
  full_join(treatment_wide, by = "id")

#---------------------------------------------------------------------------
#change classes to appropriate
medical <- medical %>% 
  mutate(
    psa = as.numeric(psa),
    BL_met_status = if_else(BL_met_status == 1, TRUE, FALSE),
    ecog = factor(ecog, levels = c("0", "1", "2", "3", "4"), ordered = TRUE), #assign ordinal levels 
    gleason_factor = case_when(
      gleason_factor %in% c("", "Not Reported") ~ NA_character_,
      TRUE ~ gleason_factor),
    gleason_factor = factor(gleason_factor, levels = c("<=6", "7", "8", "9", "10"), ordered = TRUE)
  )

#Correct timepoint variables to match remaining data sets
medical <- medical %>%
  mutate(
    timepoint = case_when(
      timepoint %in% c("Index 0", "Month 0", "Month (1)") ~ "Baseline",
      timepoint == "Baseline 3" ~ "Month 3",
      timepoint == "Baseline 6" ~ "Month 6",
      str_detect(timepoint, "Follow[- ]?Up \\d+") ~ paste0("Month ", as.numeric(str_extract(timepoint, "\\d+")) * 3),
      TRUE ~ as.character(timepoint)
    ),
    timepoint = if_else(timepoint == "Baseline 0", "Baseline", timepoint)
  )

# Assess timepoints for unique values
unique(medical$timepoint)

# Manage timepoint duplicates with different responses (keep the most complete row per participant and time point)
medical <- medical %>%
  group_by(id, timepoint) %>%
  mutate(na_count = rowSums(is.na(across(everything())))) %>%
  arrange(na_count) %>%
  slice(1) %>%
  ungroup() %>%
  select(-na_count)

#Save cleaned medical variables for merging
saveRDS(medical, here("data", "processed", "medical_cleaned.rds"))
message("Medical data processed and cleaned")