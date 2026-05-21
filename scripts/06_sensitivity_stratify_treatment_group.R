#===============================================================================
# Script: IM PA study - sensitivty analyses (stratify primary treatment group)
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
# 1. Prepare data 
#------------------------
data <- data %>%
  # generate binary treatment variable
  mutate(
    treatment_baseline_binary = case_when(
      treatment_group_baseline %in% "ADT + ARPI" ~ "ADT + ARPI",
      is.na(treatment_group_baseline) ~ NA_character_,
      TRUE ~ "Other Treatments"
      )
  )


# Create subsets by treatment group
data_ADT_ARPI <- data %>% filter(treatment_baseline_binary == "ADT + ARPI")
data_Other    <- data %>% filter(treatment_baseline_binary == "Other Treatments")

#===========================================================
# Aerobic PA guideline adherence
#===========================================================

#-----------------------
# 2. Crude models
#------------------------

# ADT + ARPI
crude_aerobic_race_ADT_ARPI <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                       (1 | id) + (1 | site),
                                     data = data_ADT_ARPI, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country_ADT_ARPI <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                          (1 | id) + (1 | site),
                                        data = data_ADT_ARPI, family = binomial,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Other Treatments
crude_aerobic_race_Other <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                    (1 | id) + (1 | site),
                                  data = data_Other, family = binomial,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country_Other <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                       (1 | id) + (1 | site),
                                     data = data_Other, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#-----------------------
# 3. Adjusted models
#------------------------


# ADT + ARPI
adjusted_aerobic_race_ADT_ARPI <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                          disease_state_baseline + age_subgroup_baseline +
                                          education_binary_baseline + ecog_binary_baseline +
                                          (1 | id) + (1 | site),
                                        data = data_ADT_ARPI, family = binomial,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country_ADT_ARPI <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                             disease_state_baseline + age_subgroup_baseline +
                                             education_binary_baseline + ecog_binary_baseline +
                                             (1 | id) + (1 | site),
                                           data = data_ADT_ARPI, family = binomial,
                                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Other Treatments
adjusted_aerobic_race_Other <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                       disease_state_baseline + age_subgroup_baseline +
                                       education_binary_baseline + ecog_binary_baseline +
                                       (1 | id) + (1 | site),
                                     data = data_Other, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country_Other <- glmer(aerobic_PA_guidelines ~ country_region + timepoint +
                                          disease_state_baseline + age_subgroup_baseline +
                                          education_binary_baseline + ecog_binary_baseline +
                                          (1 | id) + (1 | site),
                                        data = data_Other, family = binomial,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


#===========================================================
# RT guideline adherence
#===========================================================

# Filter sparse groups if needed
data_Other_filtered <- data_Other %>%
  filter(!(country_region %in% c("Other", "Africa")))

# Crude models
crude_RT_race_ADT_ARPI <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                  (1 | id) + (1 | site),
                                data = data_ADT_ARPI, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country_ADT_ARPI <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                     (1 | id) + (1 | site),
                                   data = data_ADT_ARPI, family = binomial,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_race_Other <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                               (1 | id) + (1 | site),
                             data = data_Other_filtered, family = binomial,
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country_Other <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                  (1 | id) + (1 | site),
                                data = data_Other_filtered, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Adjusted models

adjusted_RT_race_ADT_ARPI <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                     disease_state_baseline + age_subgroup_baseline +
                                     education_binary_baseline + ecog_binary_baseline +
                                     (1 | id) + (1 | site),
                                   data = data_ADT_ARPI, family = binomial,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country_ADT_ARPI <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                        disease_state_baseline + age_subgroup_baseline +
                                        education_binary_baseline + ecog_binary_baseline +
                                        (1 | id) + (1 | site),
                                      data = data_ADT_ARPI, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_race_Other <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                  disease_state_baseline + age_subgroup_baseline +
                                  education_binary_baseline + ecog_binary_baseline +
                                  (1 | id) + (1 | site),
                                data = data_Other_filtered, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country_Other <- glmer(RT_guidelines_30 ~ country_region + timepoint +
                                     disease_state_baseline + age_subgroup_baseline +
                                     education_binary_baseline + ecog_binary_baseline +
                                     (1 | id) + (1 | site),
                                   data = data_Other_filtered, family = binomial,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))



#===============================================================================
# Save key outputs as .rds files
#===============================================================================
# Define output directory
output_dir <- here("data", "processed", "stratified_treatment")
dir.create(output_dir, showWarnings = FALSE)

#-----------------------
# Crude Aerobic PA models
#-----------------------
saveRDS(crude_aerobic_race_ADT_ARPI, file = file.path(output_dir, "crude_aerobic_race_ADT_ARPI.rds"))
saveRDS(crude_aerobic_country_ADT_ARPI, file = file.path(output_dir, "crude_aerobic_country_ADT_ARPI.rds"))
saveRDS(crude_aerobic_race_Other, file = file.path(output_dir, "crude_aerobic_race_Other.rds"))
saveRDS(crude_aerobic_country_Other, file = file.path(output_dir, "crude_aerobic_country_Other.rds"))

#-----------------------
# Adjusted Aerobic PA models
#-----------------------
saveRDS(adjusted_aerobic_race_ADT_ARPI, file = file.path(output_dir, "adjusted_aerobic_race_ADT_ARPI.rds"))
saveRDS(adjusted_aerobic_country_ADT_ARPI, file = file.path(output_dir, "adjusted_aerobic_country_ADT_ARPI.rds"))
saveRDS(adjusted_aerobic_race_Other, file = file.path(output_dir, "adjusted_aerobic_race_Other.rds"))
saveRDS(adjusted_aerobic_country_Other, file = file.path(output_dir, "adjusted_aerobic_country_Other.rds"))

#-----------------------
# Crude RT models
#-----------------------
saveRDS(crude_RT_race_ADT_ARPI, file = file.path(output_dir, "crude_RT_race_ADT_ARPI.rds"))
saveRDS(crude_RT_country_ADT_ARPI, file = file.path(output_dir, "crude_RT_country_ADT_ARPI.rds"))
saveRDS(crude_RT_race_Other, file = file.path(output_dir, "crude_RT_race_Other.rds"))
saveRDS(crude_RT_country_Other, file = file.path(output_dir, "crude_RT_country_Other.rds"))

#-----------------------
# Adjusted RT models
#-----------------------
saveRDS(adjusted_RT_race_ADT_ARPI, file = file.path(output_dir, "adjusted_RT_race_ADT_ARPI.rds"))
saveRDS(adjusted_RT_country_ADT_ARPI, file = file.path(output_dir, "adjusted_RT_country_ADT_ARPI.rds"))
saveRDS(adjusted_RT_race_Other, file = file.path(output_dir, "adjusted_RT_race_Other.rds"))

#===============================================================================
# Generate publication-ready table for treatment-stratified models
#===============================================================================

# Load all saved models
model_dir <- here("data", "processed", "stratified_treatment")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
objects <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

# Keep only glmer models
models <- objects[sapply(objects, inherits, what = "glmerMod")]

# Extract fixed effects
extract_model_results <- function(model, model_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(model = model_name)
}

fixed_effects <- map2_dfr(models, names(models), extract_model_results)

# Filter subgroup terms
final_results_long <- fixed_effects %>%
  filter(str_detect(term, "^(race_simplified|country_region)")) %>%
  select(model, term, estimate, conf.low, conf.high) %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    OR_CI = paste0(estimate, " (", conf.low, " - ", conf.high, ")"),
    treatment_group = case_when(
      str_detect(model, "ADT_ARPI") ~ "ADT + ARPI",
      str_detect(model, "Other") ~ "Other Treatments"
    ),
    category = case_when(
      str_detect(model, "crude_aerobic") ~ "Crude Aerobic (OR (95% CI))",
      str_detect(model, "adjusted_aerobic") ~ "Adjusted Aerobic (OR (95% CI))",
      str_detect(model, "crude_RT") ~ "Crude RT (OR (95% CI))",
      str_detect(model, "adjusted_RT") ~ "Adjusted RT (OR (95% CI))"
    )
  ) %>%
  select(term, treatment_group, category, OR_CI)

# Combine results
final_results_long <- final_results_long %>%
  group_by(term, treatment_group, category) %>%
  summarise(OR_CI = paste(OR_CI, collapse = "; "), .groups = "drop")

# Pivot wider
final_results_clean <- final_results_long %>%
  pivot_wider(names_from = category, values_from = OR_CI)

# Reorder rows
desired_order <- c(
  "race_simplifiedBlack", "race_simplifiedOther",
  "country_regionAustralia", "country_regionNorth America", "country_regionEurope",
  "country_regionAfrica"
)

final_results_clean <- final_results_clean %>%
  mutate(term = factor(term, levels = desired_order)) %>%
  arrange(term, treatment_group) %>%
  relocate(`Crude Aerobic (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>%
  relocate(`Adjusted Aerobic (OR (95% CI))`, .after = `Crude Aerobic (OR (95% CI))`) %>%
  relocate(`Crude RT (OR (95% CI))`, .before = `Adjusted RT (OR (95% CI))`) %>%
  mutate(
    subgroup_type = case_when(
      str_starts(term, "race_simplified") ~ "Race (ref = White)",
      str_starts(term, "country_region") ~ "Geographic Region (ref = Europe)"
    ),
    term = str_remove(term, "^race_simplified|^country_region")
  ) %>%
  relocate(subgroup_type, .before = term) %>%
  rename(Subgroup = term, `Subgroup Type` = subgroup_type, `Treatment Group` = treatment_group)

#===============================================================================
# Split results by treatment group
#===============================================================================
final_results_ADT_ARPI <- final_results_clean %>%
  filter(`Treatment Group` == "ADT + ARPI") %>%
  select(-`Treatment Group`)

final_results_Other <- final_results_clean %>%
  filter(`Treatment Group` == "Other Treatments") %>%
  select(-`Treatment Group`)

#===============================================================================
# Create flextables
#===============================================================================
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

# Table for ADT + ARPI
ft_ADT_ARPI <- flextable(final_results_ADT_ARPI) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Table X. Mixed-effects logistic regression findings (Treatment Group: ADT + ARPI)",
              align_with_table = FALSE) %>%
  delete_part(part = "header") %>%
  add_header_row(
    values = c("Subgroup Type", "Subgroup", "cOR (95% CI)", "aOR (95% CI)", "cOR (95% CI)", "aOR (95% CI)"),
    colwidths = c(1, 1, 1, 1, 1, 1)
  ) %>%
  add_header_row(
    values = c("", "", "Aerobic Guidelines", "Resistance Training Guidelines"),
    colwidths = c(1, 1, 2, 2)
  ) %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  hline_top(part = "header") %>%
  hline_top(part = "body")

# Table for Other Treatments
ft_Other <- flextable(final_results_Other) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Table Y. Mixed-effects logistic regression findings (Treatment Group: Other Treatments)",
              align_with_table = FALSE) %>%
  delete_part(part = "header") %>%
  add_header_row(
    values = c("Subgroup Type", "Subgroup", "cOR (95% CI)", "aOR (95% CI)", "cOR (95% CI)", "aOR (95% CI)"),
    colwidths = c(1, 1, 1, 1, 1, 1)
  ) %>%
  add_header_row(
    values = c("", "", "Aerobic Guidelines", "Resistance Training Guidelines"),
    colwidths = c(1, 1, 2, 2)
  ) %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  hline_top(part = "header") %>%
  hline_top(part = "body")

#===============================================================================
# Save both tables to Word
#===============================================================================
save_as_docx(
  `ADT + ARPI` = ft_ADT_ARPI,
  `Other Treatments` = ft_Other,
  path = here("output", "supplemental", "sensitivity_analyses", "Table_XY_stratified_by_treatment.docx")
)
