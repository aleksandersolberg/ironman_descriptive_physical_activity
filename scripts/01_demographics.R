# ------------------------------------------------------------
# Script: Demographics extraction and cleaning 
# Author: Aleksander Solberg
# Date: 26.06.2025
# Purpose: Load, clean and save key demographic information for the IRONMAN PA Study
# ------------------------------------------------------------
#Load relevant packages
pacman::p_load(
  readxl, tidyverse, here
)

# Load and label data
proms <- read.csv(here("data", "raw", "2025-02-24_proms.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>%
  distinct()  # Remove fully duplicated rows

proms_dictionary <- read_excel(here("data", "raw", "2025-02-24_proms_dictionary.xlsx"))

#add variable labels to data set
for (var in names(proms)) {
  if (var %in% proms_dictionary$variable) {
    attr(proms[[var]], "label") <- proms_dictionary$label[proms_dictionary$variable == var]
  }
}

# Extract relevant variables
demographics <- proms %>%
  select(
    id = study_id, timepoint,
    starts_with("irondemog_3_"), starts_with("irondemog_5_"),
    starts_with("irondemog_14"), starts_with("irondemog_15_"),
    starts_with("irondemog_16"), starts_with("irondemog_17"),
    starts_with("irondemog_v3_3_"), starts_with("irondemog_v3_5_"),
    starts_with("irondemog_v3_14"), starts_with("irondemog_v3_15_"),
    starts_with("irondemog_v3_16"), starts_with("irondemog_v3_17")
  )

#------------------------------------------------------------------------------
# Extract race data from subject file
#------------------------------------------------------------------------------
# Load original subject data
subject <- read_csv(here("data", "raw", "subject.csv"))

# Extract race columns
race_cols <- subject %>%
  select(starts_with("race_26_")) %>%
  names()

# Create combined race column and row_id
subject_race <- subject %>%
  mutate(
    race_combined = pmap_chr(select(., all_of(race_cols)), ~ {
      vals <- c(...)
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0) return("No response")
      if (length(vals) > 1) return("Multiple races selected")
      return(vals)
    }),
    row_id = row_number()
  )

# Clean race values
race_cleaned <- tolower(trimws(subject_race$race_combined))
Sys.setlocale("LC_ALL", "en_US.UTF-8")

race_collapsed <- case_when(
  grepl("white|caucasian|europe|europä|svensk|vit|skandinav|deutsch|norsk|english|italian|french|maltese|mitteleurop|zentraleurop|nordbo|anglosaxon", race_cleaned, ignore.case = TRUE) ~ "White or European origin",
  grepl("african|black", race_cleaned, ignore.case = TRUE) ~ "Black or African origin",
  grepl("asian|chinese|indian|east indian|south asian", race_cleaned, ignore.case = TRUE) ~ "Asian origin",
  grepl("declined|no response|not specified|keine angabe", race_cleaned, ignore.case = TRUE) ~ NA,
  TRUE ~ "Other or Mixed"
)

# Create race_df with cleaned and simplified race
race_df <- tibble(
  row_id = subject_race$row_id,
  race_cleaned = factor(race_collapsed),
  race_simplified = factor(case_when(
    race_collapsed == "White or European origin" ~ "White",
    race_collapsed == "Black or African origin" ~ "Black",
    TRUE ~ "Other"
  ))
)

# Bind race_df back to subject
subject <- subject %>%
  mutate(row_id = row_number()) %>%
  left_join(race_df, by = "row_id") %>%
  select(-row_id)

#Create race data frame 
race <- subject %>% 
  select(id = subject, race_cleaned, race_simplified)

#Merge race to demographics
demographics <- demographics %>% 
  full_join(race, by = "id")

#----------------------------------------------------------------------------------------------------------------      
# Extract country variables from subject file and create groups
country <- subject %>% 
  select(
    id = subject, 
    site_country,
    site
  ) %>% 
  mutate(
    site_country = na_if(site_country, ""), #recode blanks to NA
    site_country = factor(site_country))

# Group countries in smaller groups
country <- country %>%
  # By income group
  mutate(
    country_income_group = case_when(
      site_country %in% c("USA", "Canada", "Australia", "England", "Ireland", "Sweden", "Norway", "Switzerland", "Spain", "Barbados") ~ "High Income",
      site_country %in% c("Brazil", "Nigeria", "Jamaica", "Kenya", "South Africa") ~ "Low- and Middle Income",
      TRUE ~ "Unknown"  # In case there's a country not classified
    ),
    country_income_group = factor(country_income_group, levels = c("High Income", "Low- and Middle Income")),  # Ensure factor levels are in order
    
    # By region
    country_region = case_when(
      site_country %in% c("USA", "Canada") ~ "North America",  # North America group
      site_country == "Australia" ~ "Australia",  # Australia group
      site_country %in% c("Jamaica", "Barbados", "Brazil") ~ "Other",
      site_country %in% c("England", "Ireland", "Sweden", "Norway", "Switzerland", "Spain") ~ "Europe",  # Europe group
      site_country %in% c("Kenya", "South Africa", "Nigeria") ~ "Africa",  # Africa group for these countries
      TRUE ~ "Unknown"  # In case there’s a country not classified
    )
  )

#Merge country to demographics
demographics <- demographics %>% 
  full_join(country, by = "id")

#----------------------------------------------------------------------------------------------------------------
#Clean living arrangement variables
# Combine multiple-choice columns and versions for living arrangement
living_cols <- c(paste0("irondemog_15_", 1:6), "irondemog_15_other", 
                 paste0("irondemog_v3_15_", 1:6), "irondemog_v3_15_other")

# Merge responses into one column
demographics <- demographics %>%
  mutate(
    living_cols_combined = pmap_chr(select(., all_of(living_cols)), ~ {
      vals <- c(...)
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0) return(NA_character_)
      paste(vals, collapse = "; ")
    })
  )

# Remove original living arrangement columns
demographics <- demographics %>%
  select(-all_of(living_cols))

#Create meaningful categories for living arrangement
demographics <- demographics %>%
  
  mutate(
    living_arrangement_collapsed = case_when(
      # Preserve original NAs
      is.na(living_cols_combined) ~ NA_character_,
      
      # Clear "partner" references
      grepl("wife|partner|fiance|girlfriend|with my wife|with wife/partner", living_cols_combined, ignore.case = TRUE) ~ "With partner",
      
      # Alone
      grepl("\\balone\\b", living_cols_combined, ignore.case = TRUE) ~ "Alone",
      
      # Family, excluding partner
      grepl("family|children|daughter|son|custody|avec fils", living_cols_combined, ignore.case = TRUE) ~ "With family (not partner)",
      
      # Institutional / care environments
      grepl("assisted living|nursing home|retirement community|rehab|rest home|shelter|institution|comunidade religiosa", living_cols_combined, ignore.case = TRUE) ~ "Assisted or institutional living",
      
      # Explicit not specified
      grepl("not specified", living_cols_combined, ignore.case = TRUE) ~ "Missing",
      
      # Fallback
      TRUE ~ "Other"
    )
  )

#----------------------------------------------------------------------------------------------------------------      
#Clean education variables    
# Combine education-related columns from different versions
edu_vars <- c("irondemog_16", "irondemog_16_other", 
              "irondemog_v3_16", "irondemog_v3_16_other")  

# Merge responses into a single education column
demographics <- demographics %>%
  mutate(
    education_combined = pmap_chr(select(., all_of(edu_vars)), ~ {
      vals <- c(...)
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0) return(NA_character_)
      paste(vals, collapse = "; ")
    })
  )

# Remove redundant education columns
demographics <- demographics %>%
  select(-all_of(edu_vars))

categorize_education <- function(edu) {
  if (!is.na(edu) && str_trim(edu) == "") return("missing")
  if (is.na(edu)) return(NA_character_)
  
  edu_lower <- tolower(edu)
  
  if (edu_lower %in% c("no formal education", "none")) return("No Formal Education")
  if (grepl("secondary|high school", edu_lower)) return("Secondary Education")
  if (grepl("college|university|bachelor|master|phd", edu_lower)) return("Higher Education")
  
  return("Other")
}

# Apply categorization
demographics <- demographics %>%
  mutate(education_category = sapply(education_combined, categorize_education)
  )

#----------------------------------------------------------------------------------------------------------------
# Clean marital and employment status variables
combine_vars <- function(demo_df, v2_vars, v3_vars) {
  for (i in seq_along(v2_vars)) {
    v2 <- v2_vars[i]
    v3 <- v3_vars[i]
    combined_name <- gsub("irondemog(_v3)?_", "irondemog_combined_", v2)
    
    if (v2 %in% names(demo_df) && v3 %in% names(demo_df)) {
      message("Combining ", v2, " and ", v3)
      message("  Non-missing in ", v2, ": ", sum(!is.na(demo_df[[v2]])))
      message("  Non-missing in ", v3, ": ", sum(!is.na(demo_df[[v3]])))
      
      demo_df <- demo_df %>%
        mutate(!!sym(combined_name) := coalesce(
          na_if(trimws(as.character(.data[[v2]])), ""),
          na_if(trimws(as.character(.data[[v3]])), "")
        ))
      
      message("  Non-missing in combined: ", sum(!is.na(demo_df[[combined_name]])))
    } else {
      warning("Skipping pair: ", v2, " or ", v3, " not found in data.")
    }
  }
  return(demo_df)
}

#Marital status
v2_vars_marital <- c("irondemog_14")
v3_vars_marital <- c("irondemog_v3_14")

demographics <- combine_vars(demographics, v2_vars_marital, v3_vars_marital)
demographics <- demographics %>%
  select(-all_of(c(v2_vars_marital, v3_vars_marital)))

#Employement status
v2_vars_employment <- c("irondemog_17")
v3_vars_employment <- c("irondemog_v3_17")
demographics <- combine_vars(demographics, v2_vars_employment, v3_vars_employment)

# Remove original employment status variables
demographics <- demographics %>%
  select(-all_of(c(v2_vars_employment, v3_vars_employment)))
#-----------------------------------------------------------------------------          
#Clean and process weight and height data
# Extract weigth and height data 
v2_vars_group1 <- c("irondemog_3_1", "irondemog_3_2", "irondemog_5_1", "irondemog_5_2")
v3_vars_group1 <- c("irondemog_v3_3_1", "irondemog_v3_3_2", "irondemog_v3_5_1", "irondemog_v3_5_2")

# Combine weight and height variables
demographics <- combine_vars(demographics, v2_vars_group1, v3_vars_group1)

# Rename weight and height variables for clarity
demographics <- demographics %>%
  rename(
    height_inches = irondemog_combined_3_1,
    height_cm = irondemog_combined_3_2,
    weight_lbs = irondemog_combined_5_1,
    weight_kg = irondemog_combined_5_2)

#calculate BMI from reported height and weight (account for differences in units)
#Ensure data is numeric  
demographics <- demographics %>%
  mutate(across(c(weight_kg, height_cm, weight_lbs, height_inches), as.numeric))

#Adress extreme values
summary(demographics[, c("weight_kg", "height_cm", "weight_lbs", "height_inches")])

# Flag extreme values based on plausible human ranges
demographics$extreme_weight_kg     <- demographics$weight_kg < 30 | demographics$weight_kg > 250
demographics$extreme_height_cm     <- demographics$height_cm < 120 | demographics$height_cm > 230
demographics$extreme_weight_lbs    <- demographics$weight_lbs < 66 | demographics$weight_lbs > 550
demographics$extreme_height_inches <- demographics$height_inches < 47 | demographics$height_inches > 91

extreme_rows <- demographics %>%
  filter(
    extreme_weight_kg | extreme_height_cm |
      extreme_weight_lbs | extreme_height_inches
  )

#Based on findings from extreme rows, conduct cleaning:
# 1. Replace weight_kg under 50 with NA
demographics$weight_kg[demographics$weight_kg < 50] <- NA

# 2. Replace weight_lbs under 140 with NA
demographics$weight_lbs[demographics$weight_lbs < 140] <- NA

# 3. Replace height_cm between 235 and 1000 with NA
demographics$height_cm[demographics$height_cm >= 235 & demographics$height_cm < 1000] <- NA

# 4. If height_cm > 1000, divide by 10
correct_idx <- which(!is.na(demographics$height_cm) & demographics$height_cm > 1000)
message("Corrected ", length(correct_idx), " height_cm values > 1000")

# 5. Replace height_cm < 130 with NA
demographics$height_cm[!is.na(demographics$height_cm) & demographics$height_cm < 130] <- NA

# 6. Replace height_inches < 40 with NA
demographics$height_inches[!is.na(demographics$height_inches) & demographics$height_inches < 40] <- NA

# 7. If height_inches > 500, divide by 10
fix_inches <- which(!is.na(demographics$height_inches) & demographics$height_inches > 500)
demographics$height_inches[fix_inches] <- demographics$height_inches[fix_inches] / 10

# 8. Move values between 80–230 from height_inches to height_cm
move_to_cm <- !is.na(demographics$height_inches) & demographics$height_inches >= 80 & demographics$height_inches <= 230
demographics$height_cm[move_to_cm & is.na(demographics$height_cm)] <- demographics$height_inches[move_to_cm]
demographics$height_inches[move_to_cm] <- NA

#Reassess for extreme values
summary(demographics[, c("weight_kg", "height_cm", "weight_lbs", "height_inches")])

# Calculate BMI and flag implausible values
demographics <- demographics %>%
  mutate(
    BMI = case_when(
      !is.na(weight_kg) & !is.na(height_cm) ~ weight_kg / ((height_cm / 100) ^ 2),
      !is.na(weight_lbs) & !is.na(height_inches) ~ (weight_lbs * 0.45359237) / ((height_inches * 0.0254) ^ 2),
      TRUE ~ NA_real_
    ),
    implausible_BMI = BMI < 10 | BMI > 60
  ) %>%
  
  # Remove implausible BMI values
  filter(!implausible_BMI | is.na(implausible_BMI)) %>%
  # Drop temporary columns
  select(-implausible_BMI, -weight_kg, -height_cm, -weight_lbs, -height_inches,
         -extreme_weight_kg, -extreme_height_cm, -extreme_weight_lbs, -extreme_height_inches)

#-------------------------------------------------------------------------------        
# Extract and merge age information from dm file to demographics
dm <- read.csv(here("data", "raw", "dm.csv"))  

dm_age <- dm %>% 
  select(id = subject, age) %>% 
  mutate(age = as.numeric(age))

demographics <- demographics %>% 
  full_join(dm_age, by = "id")

# Rename columns and convert chr. variables to factor
demographics <- demographics %>%
  rename(
    race = race_cleaned,
    living_arrangement = living_arrangement_collapsed,
    education = education_category,
    marital_status = irondemog_combined_14,
    employment = irondemog_combined_17
  ) %>% 
  mutate(
    living_arrangement = factor(living_arrangement),
    education = factor(education),
    marital_status = factor(marital_status),
    employment = factor(employment),
    country_region = factor(country_region)
  )


# Extract relevant study variables
demographics <- demographics %>% 
  select(id, timepoint, site, site_country, country_income_group, country_region, race_simplified, living_arrangement, education, marital_status,
         employment, BMI, age)

#Save to cleaned files directory
saveRDS(demographics, here("data", "processed", "demographics.rds"))