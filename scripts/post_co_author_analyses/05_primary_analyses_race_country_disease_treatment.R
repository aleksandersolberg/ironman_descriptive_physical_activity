#===============================================================================
# Script: IM PA study - primary analyses
# Author: Aleksander Solberg
# Purpose: Analyse how PA levels differ by race and country region with and without adjustments. 
#===============================================================================
#-----------------------
# 1. Prepare environment
#------------------------
# Load required packages
pacman::p_load(tidyverse, here, lme4, emmeans, broom.mixed, flextable, gtsummary)

# Load processed dataset
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))

#-----------------------
# 2. Define variables
#------------------------
# Define outcomes of interest
outcome_vars <- c("aerobic_PA_guidelines", "RT_guidelines_30")

# Define sub-groups and covariates
adj_vars <- c("race_simplified", "country_region", "disease_state_baseline", 
              "treatment_group_baseline", "age_subgroup_baseline", "education_binary_baseline",
              "ecog_binary_baseline")

#-----------------------
# 3. Prepare baseline covariates 
#------------------------
baseline_vars <- data_filtered %>%
  filter(timepoint == "Baseline") %>%
  select(id, disease_state, treatment_group, age_subgroup, education_binary, ecog_binary
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  rename_with(~ paste0(.x, "_baseline"), -id)

#-----------------------
# 4. Prepare dataset for analysis
#------------------------
data <- data_filtered %>%
  filter(timepoint != "Baseline") %>% #Filter out baseline timepoint (no data)
  #Merge outcome data with baseline covariates
  left_join(
    baseline_vars %>%
      select(-any_of(c("aerobic_PA_guidelines", "RT_guidelines_30"))),
    by = "id"
  ) %>%
  mutate(
    across(c(aerobic_PA_guidelines, RT_guidelines_30), as.factor) # Convert outcomes to factors
  ) %>%
  mutate(across(all_of(adj_vars), ~ fct_na_value_to_level(.x, level = "Missing"))) # Handle missing group values

# Filter out the "other" group from country region due to sparse outcomes
data <- data %>%
  filter(!(country_region %in% "Other"))


#=====================================
#Run analyses overall (mixed effects model with timepoint as fixed effect, id and study sites as random effects)
#==================================
# Aerobic PA guidelines
# Crude models
crude_aerobic_race <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint + (1 | id) + (1 | site),
                            data = data, family = binomial,
                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country <- glmer(aerobic_PA_guidelines ~ country_region + timepoint + (1 | id) + (1 | site),
                               data = data, family = binomial,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_disease <- glmer(
  aerobic_PA_guidelines ~ disease_state_baseline + timepoint +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

crude_aerobic_treatment <- glmer(
  aerobic_PA_guidelines ~ treatment_group_baseline + timepoint +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

adjusted_aerobic_race <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                 disease_state_baseline + age_subgroup_baseline +
                                 education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                                 (1 | id) + (1 | site),
                               data = data, family = binomial,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                    disease_state_baseline + age_subgroup_baseline +
                                    education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                                    (1 | id) + (1 | site),
                                  data = data, family = binomial,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_disease <- glmer(
  aerobic_PA_guidelines ~ disease_state_baseline + timepoint +
    age_subgroup_baseline + education_binary_baseline +
    ecog_binary_baseline +
    race_simplified + country_region +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)


adjusted_aerobic_treatment <- glmer(
  aerobic_PA_guidelines ~ treatment_group_baseline + timepoint +
    disease_state_baseline + age_subgroup_baseline +
    education_binary_baseline + ecog_binary_baseline +
    race_simplified + country_region +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)


# RT guidelines
crude_RT_race <- glmer(RT_guidelines_30 ~ race_simplified + timepoint + (1 | id) + (1 | site),
                       data = data, family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country <- glmer(RT_guidelines_30 ~ country_region + timepoint + (1 | id) + (1 | site),
                          data = data, family = binomial,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


crude_RT_disease <- glmer(
  RT_guidelines_30 ~ disease_state_baseline + timepoint +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

crude_RT_treatment <- glmer(
  RT_guidelines_30 ~ treatment_group_baseline + timepoint +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

adjusted_RT_race <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                            disease_state_baseline + age_subgroup_baseline +
                            education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                            (1 | id) + (1 | site),
                          data = data, family = binomial,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                               disease_state_baseline + age_subgroup_baseline +
                               education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                               (1 | id) + (1 | site),
                             data = data, family = binomial,
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


adjusted_RT_disease <- glmer(
  RT_guidelines_30 ~ disease_state_baseline + timepoint +
    age_subgroup_baseline + education_binary_baseline +
    ecog_binary_baseline +
    race_simplified + country_region +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

adjusted_RT_treatment <- glmer(
  RT_guidelines_30 ~ treatment_group_baseline + timepoint +
    disease_state_baseline + age_subgroup_baseline +
    education_binary_baseline + ecog_binary_baseline +
    race_simplified + country_region +
    (1 | id) + (1 | site),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)


#----------------------------------------
#Save overall output (saves time when editting tables)
#---------------------------------------
#Define output directory
output_dir <- here("data", "processed", "overall")

# Save all models
saveRDS(crude_aerobic_race, file = file.path(output_dir, "crude_aerobic_race.rds"))
saveRDS(crude_aerobic_country, file = file.path(output_dir, "crude_aerobic_country.rds"))
saveRDS(adjusted_aerobic_race, file = file.path(output_dir, "adjusted_aerobic_race.rds"))
saveRDS(adjusted_aerobic_country, file = file.path(output_dir, "adjusted_aerobic_country.rds"))
saveRDS(crude_RT_race, file = file.path(output_dir, "crude_RT_race.rds"))
saveRDS(crude_RT_country, file = file.path(output_dir, "crude_RT_country.rds"))
saveRDS(adjusted_RT_race, file = file.path(output_dir, "adjusted_RT_race.rds"))
saveRDS(adjusted_RT_country, file = file.path(output_dir, "adjusted_RT_country.rds"))
saveRDS(adjusted_aerobic_disease, file.path(output_dir, "adjusted_aerobic_disease.rds"))
saveRDS(adjusted_aerobic_treatment, file.path(output_dir, "adjusted_aerobic_treatment.rds"))
saveRDS(adjusted_RT_disease, file.path(output_dir, "adjusted_RT_disease.rds"))
saveRDS(adjusted_RT_treatment, file.path(output_dir, "adjusted_RT_treatment.rds"))
saveRDS(crude_aerobic_disease, file.path(output_dir, "crude_aerobic_disease.rds"))
saveRDS(crude_aerobic_treatment, file.path(output_dir, "crude_aerobic_treatment.rds"))
saveRDS(crude_RT_disease, file.path(output_dir, "crude_RT_disease.rds"))
saveRDS(crude_RT_treatment, file.path(output_dir, "crude_RT_treatment.rds"))

# Save analysis dataset for sensitivity analyses later
saveRDS(data, here("data", "processed", "analysis_data.rds"))
#-----------------------
# Generate table
#-----------------------
#  Load all saved objects
model_dir <- here("data", "processed", "overall")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
objects <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

# Keep only objects that are glmer models
models <- objects[sapply(objects, inherits, what = "glmerMod")]

# Specify functions to extract results
  # Function to extract fixed effects with confidence intervals
  extract_model_results <- function(model, model_name) {
    # Use broom.mixed::tidy to get estimates and CIs
    broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(model = model_name)
  }
  
  # Function to extract random effects (variance components)
  extract_random_effects <- function(model, model_name) {
    broom.mixed::tidy(model, effects = "ran_pars", conf.int = TRUE) %>%
      mutate(model = model_name)
  }

#  Apply extraction functions
fixed_effects <- map2_dfr(models, names(models), extract_model_results)
random_effects <- map2_dfr(models, names(models), extract_random_effects)

# Combine and save into one dataframe
final_results <- bind_rows(fixed_effects, random_effects)

# Select only variables of interest
final_results <- final_results %>% 
  filter(str_detect(
    model,
    "^(crude|adjusted)_(aerobic|RT)_(race|country|disease|treatment)"
  ))
  
  # Extract random effects into separate df
  final_results_sd_random <- final_results %>% 
    filter(group == "id")
  
# Tidy output
  
  final_results_long <- final_results %>%
    filter(term != "id") %>%
    filter(str_detect(
      term,
      "^(race_simplified|country_region|disease_state_baseline|treatment_group_baseline)"
    )) %>%
    mutate(
      # Convert log-odds to OR
      estimate = exp(as.numeric(estimate)),
      conf.low = exp(as.numeric(conf.low)),
      conf.high = exp(as.numeric(conf.high)),
      estimate = round(estimate, 2),
      conf.low = round(conf.low, 2),
      conf.high = round(conf.high, 2),
      OR_CI = ifelse(
        is.na(estimate),
        "",
        paste0(estimate, " (", conf.low, " - ", conf.high, ")")
      ),
      # Model category
      category = case_when(
        str_detect(model, "crude_aerobic") ~ "Crude Aerobic (OR (95% CI))",
        str_detect(model, "adjusted_aerobic") ~ "Adjusted Aerobic (OR (95% CI))",
        str_detect(model, "adjusted_RT") ~ "Adjusted RT (OR (95% CI))",
        str_detect(model, "crude_RT") ~ "Crude RT (OR (95% CI))"
      )
    ) %>%
    select(model, term, category, OR_CI)

final_results_long <- final_results_long %>%
  mutate(
    model_owner = case_when(
      str_detect(term, "^race_simplified") & str_detect(model, "_race$") ~ TRUE,
      str_detect(term, "^country_region") & str_detect(model, "_country$") ~ TRUE,
      str_detect(term, "^disease_state_baseline") & str_detect(model, "_disease$") ~ TRUE,
      str_detect(term, "^treatment_group_baseline") & str_detect(model, "_treatment$") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(model_owner) %>%
  select(-model)

final_results_long <- final_results_long %>%
  group_by(term, category) %>%
  summarise(OR_CI = paste(OR_CI, collapse = "; "), .groups = "drop")

# Pivot wider by category, keeping disease_state separate
final_results_clean <- final_results_long %>%
  pivot_wider(names_from = category, values_from = OR_CI)

# Reorder rows

desired_order <- c(
  # Race
  "race_simplifiedBlack", "race_simplifiedOther",
  
  # Region
  "country_regionAustralia", "country_regionNorth America",
  "country_regionEurope", "country_regionAfrica",
  
  # Disease state
  "disease_state_baselineCRPC",
  
  # Treatment group
  "treatment_group_baselineADT + ARPI + chemo",
  "treatment_group_baselineADT + chemo",
  "treatment_group_baselineADT monotherapy"
)

# Arrange for readability
final_results_clean <- final_results_clean %>%
  arrange(term) %>% 
  mutate(term = factor(term, levels = desired_order)) %>%
  filter(!is.na(term)) %>%   # <-- DROP UNWANTED ROWS
  arrange(term) %>%
  relocate(`Crude Aerobic (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>%
  relocate(`Adjusted Aerobic (OR (95% CI))`, .after = `Crude Aerobic (OR (95% CI))`) %>%
  relocate(`Crude RT (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`)

#===============================================================================
# 10. Create table for overall results
#===============================================================================
final_results_overall <- final_results_clean %>% 
  mutate(term = factor(term, levels = desired_order)) %>%
  arrange(term) %>%
  relocate(`Crude Aerobic (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>%
  relocate(`Adjusted Aerobic (OR (95% CI))`, .after = `Crude Aerobic (OR (95% CI))`) %>%
  relocate(`Crude RT (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>% 
  
  mutate(
    subgroup_type = case_when(
      str_starts(term, "race_simplified") ~ "Race (ref = White)",
      str_starts(term, "country_region") ~ "Geographic Region (ref = Europe)",
      str_starts(term, "disease_state_baseline") ~ "Disease State (ref = mHSPC)",
      str_starts(term, "treatment_group_baseline") ~ "First On-Study Treatment Regimen (ref = ADT + ARPI)"
    ),
    term = str_remove(
      term,
      "^race_simplified|^country_region|^disease_state_baseline|^treatment_group_baseline"
    )
  ) %>% 
  relocate(subgroup_type, .before = term) %>% 
  rename(Subgroup = term,
         `Subgroup Type` = subgroup_type)


set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

ft_all <- flextable(final_results_overall) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Mixed effects log. reg findings",
              align_with_table = FALSE) %>%
  delete_part(part = "header") %>%  # Remove old header
  add_header_row(
    values = c("Subgroup Type", "Subgroup", "cOR (95% CI)", "aOR (95% CI)", "cOR (95% CI)", "aOR (95% CI)"),
    colwidths = c(1, 1, 1, 1, 1, 1)
  ) %>% 
  add_header_row(
    values = c("", "", "Aerobic Guidelines", "Resistance Training Guidelines"),
    colwidths = c(1, 1, 2, 2)
  ) %>%
  bold(part = "header") %>%  # Apply bold AFTER adding headers
  align(align = "center", part = "header") %>% 
  hline_top(part = "header") %>% 
  hline_top(part = "body")  

# Display table
ft_all

# Save to Word
save_as_docx(
  ft_all,
  path = here("output", "Table 3.docx")
)