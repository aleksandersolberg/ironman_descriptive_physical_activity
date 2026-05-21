#===============================================================================
# Script: IM PA study - sensitivty analyses (stratify disease state)
# Author: Aleksander Solberg
#===============================================================================
#-----------------------
# 1. Prepare environment
#------------------------
pacman::p_load(tidyverse, here, lme4, emmeans, broom.mixed,  flextable)

# Load analysis dataset
data <- readRDS(here("data", "processed", "analysis_data.rds"))

#===========================================================
# Stratified analyses by disease state
#===========================================================

#-----------------------
# 1. Split data by disease state
#------------------------
data <- data %>%
  filter(!(country_region %in% "Other"))

data_mHSPC <- data %>% filter(disease_state == "mHSPC")
data_CRPC  <- data %>% filter(disease_state == "CRPC")

#===========================================================
# Aerobic PA guideline adherence
#===========================================================

#-----------------------
# 2. Crude models
#------------------------

# mHSPC
crude_aerobic_race_mHSPC <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                    (1 | id) + (1 | site),
                                  data = data_mHSPC, family = binomial,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country_mHSPC <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                       (1 | id) + (1 | site),
                                     data = data_mHSPC, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# CRPC
crude_aerobic_race_CRPC <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                   (1 | id) + (1 | site),
                                 data = data_CRPC, family = binomial,
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country_CRPC <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                      (1 | id) + (1 | site),
                                    data = data_CRPC, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#-----------------------
# 3. Adjusted models
#------------------------

# mHSPC
adjusted_aerobic_race_mHSPC <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                       age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                       treatment_group_baseline + 
                                       (1 | id) + (1 | site),
                                     data = data_mHSPC, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country_mHSPC <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                          age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                          treatment_group_baseline +
                                          (1 | id) + (1 | site),
                                        data = data_mHSPC, family = binomial,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# CRPC
adjusted_aerobic_race_CRPC <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                      age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                      treatment_group_baseline +
                                      (1 | id) + (1 | site),
                                    data = data_CRPC, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country_CRPC <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                         age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                         treatment_group_baseline +
                                         (1 | id) + (1 | site),
                                       data = data_CRPC, family = binomial,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#===========================================================
# RT guideline adherence
#===========================================================

# Filter CRPC for sparse groups
data_CRPC_filtered <- data_CRPC %>%
  filter(!(country_region %in% c("Other", "Africa")))

# Crude models
crude_RT_race_mHSPC <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                               (1 | id) + (1 | site),
                             data = data_mHSPC, family = binomial,
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country_mHSPC <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                  (1 | id) + (1 | site),
                                data = data_mHSPC, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_race_CRPC <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                              (1 | id) + (1 | site),
                            data = data_CRPC_filtered, family = binomial,
                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country_CRPC <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                 (1 | id) + (1 | site),
                               data = data_CRPC_filtered, family = binomial,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Adjusted models
adjusted_RT_race_mHSPC <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                  age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                  (1 | id) + (1 | site),
                                data = data_mHSPC, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country_mHSPC <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                     age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                     (1 | id) + (1 | site),
                                   data = data_mHSPC, family = binomial,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_race_CRPC <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                 age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                 (1 | id) + (1 | site),
                               data = data_CRPC_filtered, family = binomial,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country_CRPC <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                    age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline +
                                    (1 | id) + (1 | site),
                                  data = data_CRPC_filtered, family = binomial,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#===============================================================================
# Save key outputs as .rds files
#===============================================================================
output_dir <- here("data", "processed", "stratified")


#===============================================================================
# Save key outputs as .rds files
#===============================================================================
# Define output directory
output_dir <- here("data", "processed", "stratified")

#-----------------------
# Crude Aerobic PA models
#-----------------------
saveRDS(crude_aerobic_race_mHSPC, file = file.path(output_dir, "crude_aerobic_race_mHSPC.rds"))
saveRDS(crude_aerobic_country_mHSPC, file = file.path(output_dir, "crude_aerobic_country_mHSPC.rds"))
saveRDS(crude_aerobic_race_CRPC, file = file.path(output_dir, "crude_aerobic_race_CRPC.rds"))
saveRDS(crude_aerobic_country_CRPC, file = file.path(output_dir, "crude_aerobic_country_CRPC.rds"))

#-----------------------
# Adjusted Aerobic PA models
#-----------------------
saveRDS(adjusted_aerobic_race_mHSPC, file = file.path(output_dir, "adjusted_aerobic_race_mHSPC.rds"))
saveRDS(adjusted_aerobic_country_mHSPC, file = file.path(output_dir, "adjusted_aerobic_country_mHSPC.rds"))
saveRDS(adjusted_aerobic_race_CRPC, file = file.path(output_dir, "adjusted_aerobic_race_CRPC.rds"))
saveRDS(adjusted_aerobic_country_CRPC, file = file.path(output_dir, "adjusted_aerobic_country_CRPC.rds"))

#-----------------------
# Crude RT models
#-----------------------
saveRDS(crude_RT_race_mHSPC, file = file.path(output_dir, "crude_RT_race_mHSPC.rds"))
saveRDS(crude_RT_country_mHSPC, file = file.path(output_dir, "crude_RT_country_mHSPC.rds"))
saveRDS(crude_RT_race_CRPC, file = file.path(output_dir, "crude_RT_race_CRPC.rds"))
saveRDS(crude_RT_country_CRPC, file = file.path(output_dir, "crude_RT_country_CRPC.rds"))

#-----------------------
# Adjusted RT models
#-----------------------
saveRDS(adjusted_RT_race_mHSPC, file = file.path(output_dir, "adjusted_RT_race_mHSPC.rds"))
saveRDS(adjusted_RT_country_mHSPC, file = file.path(output_dir,"adjusted_RT_country_mHSPC.rds"))
saveRDS(adjusted_RT_race_CRPC, file = file.path(output_dir, "adjusted_RT_race_CRPC.rds"))
saveRDS(adjusted_RT_country_CRPC, file = file.path(output_dir, "adjusted_RT_country_CRPC.rds"))

#-----------------------
# Generate publication ready table
#-----------------------
#  Load all saved objects
model_dir <- here("data", "processed", "stratified")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
objects <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

# Keep only objects that are glmer models
models <- objects[sapply(objects, inherits, what = "glmerMod")]

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

#  Apply extraction functions safely
fixed_effects <- map2_dfr(models, names(models), extract_model_results)
random_effects <- map2_dfr(models, names(models), extract_random_effects)

# Combine and save
final_results <- bind_rows(fixed_effects, random_effects)

final_results <- final_results %>%
  filter(model %in% c(
    # Adjusted models
    "adjusted_RT_race_mHSPC",
    "adjusted_RT_race_CRPC",
    "adjusted_aerobic_race_mHSPC",
    "adjusted_aerobic_race_CRPC",
    "adjusted_RT_country_mHSPC",
    "adjusted_RT_country_CRPC",
    "adjusted_aerobic_country_mHSPC",
    "adjusted_aerobic_country_CRPC",
    
    # Crude RT models
    "crude_RT_race_mHSPC",
    "crude_RT_country_mHSPC",
    "crude_RT_race_CRPC",
    "crude_RT_country_CRPC",
    
    # Crude Aerobic models
    "crude_aerobic_race_mHSPC",
    "crude_aerobic_country_mHSPC",
    "crude_aerobic_race_CRPC",
    "crude_aerobic_country_CRPC"
  ))


# Extract random effects into separate df
final_results_sd_random <- final_results %>% 
  filter(group == "id")

# Create tidy results df
final_results_long <- final_results %>%
  filter(term != "id") %>%
  filter(str_detect(term, "^(race_simplified|country_region)")) %>%
  select(model, term, estimate, conf.low, conf.high) %>%
  mutate(
    # Convert log-odds to OR
    estimate = exp(as.numeric(estimate)),
    conf.low = exp(as.numeric(conf.low)),
    conf.high = exp(as.numeric(conf.high)),
    # Round after exponentiation
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    OR_CI = ifelse(is.na(estimate), "",
                   paste0(estimate, " (", conf.low, " - ", conf.high, ")")),
    disease_state = case_when(
      str_detect(model, "mHSPC") ~ "mHSPC",
      str_detect(model, "CRPC") ~ "CRPC"
    ),
    category = case_when(
      str_detect(model, "crude_aerobic") ~ "Crude Aerobic (OR (95% CI))",
      str_detect(model, "adjusted_aerobic") ~ "Adjusted Aerobic (OR (95% CI))",
      str_detect(model, "crude_RT") ~ "Crude RT (OR (95% CI))",
      str_detect(model, "adjusted_RT") ~ "Adjusted RT (OR (95% CI))"
    )
  ) %>%
  select(term, disease_state, category, OR_CI)


final_results_long <- final_results_long %>%
  group_by(term, disease_state, category) %>%
  summarise(OR_CI = paste(OR_CI, collapse = "; "), .groups = "drop")

# Pivot wider by category, keeping disease_state separate
final_results_clean <- final_results_long %>%
  pivot_wider(names_from = category, values_from = OR_CI)

# Reorder rows
# Example vector of terms
desired_order <- c(
  "race_simplifiedBlack", "race_simplifiedOther",  # race first
  "country_regionAustralia", "country_regionNorth America", "country_regionEurope",
  "country_regionAfrica", "country_regionOther"            # Africa & Other last
)

# Arrange for readability
final_results_clean <- final_results_clean %>%
  arrange(term, disease_state) %>% 
  mutate(term = factor(term, levels = desired_order)) %>%
  arrange(term) %>%
  relocate(`Crude Aerobic (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>%
  relocate(`Adjusted Aerobic (OR (95% CI))`, .after = `Crude Aerobic (OR (95% CI))`) %>%
  relocate(`Crude RT (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>% 
  mutate(
    subgroup_type = case_when(
      str_starts(term, "race_simplified") ~ "Race (ref = White)",
      str_starts(term, "country_region") ~ "Geographic Region (ref = Europe)"
    ),
    term = str_remove(term, "^race_simplified|^country_region")) %>% 
  relocate(subgroup_type, .before = term) %>% 
  rename(Subgroup = term,
         `Subgroup Type` = subgroup_type)

#===============================================================================
# 10. Split by disease state into two tables
#===============================================================================
final_results_mHSPC <- final_results_clean %>%
  filter(disease_state == "mHSPC") %>%
  select(-disease_state) 

final_results_CRPC <- final_results_clean %>%
  filter(disease_state == "CRPC") %>%
  select(-disease_state)

set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

ft_mHSPC <- flextable(final_results_mHSPC) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Table 2. Mixed effects log. reg findings mHSPC",
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
  align(align = "center", part = "header")  %>% 
  hline_top(part = "header") %>% 
  hline_top(part = "body")  

ft_mHSPC

# Save to Word
save_as_docx(
  ft_mHSPC,
  path = here("output", "supplemental", "sensitivity_analyses", "Table 3a mixed_effects mHSPC.docx")
)

ft_CRPC <- flextable(final_results_CRPC) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Table 2. Mixed effects log. reg findings CRPC",
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

ft_CRPC

# Save to Word
save_as_docx(
  ft_CRPC,
  path = here("output", "supplemental", "sensitivity_analyses", "Table 3b mixed_effects mCRPC.docx")
)
