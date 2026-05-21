#===============================================================================
# Script: IM PA study - sensitivity analyses - remove Month 30
# Author: Aleksander Solberg
#===============================================================================

pacman::p_load(tidyverse, here, lme4, broom.mixed, flextable, gtsummary, officer)

# Load dataset
data <- readRDS(here("data", "processed", "analysis_data.rds"))

# Remove Month 30
data_timepoint_filtered <- data %>%
  filter(timepoint != "Month 30")

#-----------------------------------
# Run models
#-----------------------------------
# Aerobic PA guidelines
crude_aerobic_race_timepoint <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint + (1 | id) + (1 | site),
                                      data = data_timepoint_filtered, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_race_timepoint <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint + disease_state_baseline +
                                           age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline + treatment_group_baseline + (1 | id)+ (1 | site),
                                         data = data_timepoint_filtered, family = binomial,
                                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_aerobic_country_timepoint <- glmer(aerobic_PA_guidelines ~ country_region + timepoint + (1 | id) + (1 | site),
                                         data = data_timepoint_filtered, family = binomial,
                                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_aerobic_country_timepoint <- glmer(aerobic_PA_guidelines ~ country_region + timepoint + disease_state_baseline +
                                              age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline + treatment_group_baseline + (1 | id) + (1 | site),
                                            data = data_timepoint_filtered, family = binomial,
                                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# RT guidelines
crude_RT_race_timepoint <- glmer(RT_guidelines_30 ~ race_simplified + timepoint + (1 | id) + (1 | site),
                                 data = data_timepoint_filtered, family = binomial,
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_race_timepoint <- glmer(RT_guidelines_30 ~ race_simplified + timepoint + disease_state_baseline +
                                      age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline + treatment_group_baseline + (1 | id) + (1 | site),
                                    data = data_timepoint_filtered, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

crude_RT_country_timepoint <- glmer(RT_guidelines_30 ~ country_region + timepoint + (1 | id) + (1 | site),
                                    data = data_timepoint_filtered, family = binomial,
                                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_country_timepoint <- glmer(RT_guidelines_30 ~ country_region + timepoint + disease_state_baseline +
                                         age_subgroup_baseline + education_binary_baseline + ecog_binary_baseline + treatment_group_baseline + (1 | id) + (1 | site),
                                       data = data_timepoint_filtered, family = binomial,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#-----------------------------------
# Save models
#-----------------------------------
save_dir <- here("data", "processed", "sensitivity_timepoint")
dir.create(save_dir, showWarnings = FALSE)

saveRDS(crude_aerobic_race_timepoint, file.path(save_dir, "crude_aerobic_race.rds"))
saveRDS(adjusted_aerobic_race_timepoint, file.path(save_dir, "adjusted_aerobic_race.rds"))
saveRDS(crude_aerobic_country_timepoint, file.path(save_dir, "crude_aerobic_country.rds"))
saveRDS(adjusted_aerobic_country_timepoint, file.path(save_dir, "adjusted_aerobic_country.rds"))
saveRDS(crude_RT_race_timepoint, file.path(save_dir, "crude_RT_race.rds"))
saveRDS(adjusted_RT_race_timepoint, file.path(save_dir, "adjusted_RT_race.rds"))
saveRDS(crude_RT_country_timepoint, file.path(save_dir, "crude_RT_country.rds"))
saveRDS(adjusted_RT_country_timepoint, file.path(save_dir, "adjusted_RT_country.rds"))

#---------------------------------------
# Extract and pool results to CSV
#---------------------------------------
model_dir <- here("data", "processed", "sensitivity_timepoint")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
models <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

extract_or_ci <- function(model, model_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      OR = exp(estimate),
      CI_low = exp(conf.low),
      CI_high = exp(conf.high),
      OR_CI = paste0(round(OR, 2), " (", round(CI_low, 2), " - ", round(CI_high, 2), ")"),
      model = model_name
    ) %>%
    select(model, term, OR_CI, p.value)
}

results <- map2_dfr(models, names(models), extract_or_ci)

# ✅ Filter only subgroup terms (race or country_region)
results_subgroup <- results %>%
  filter(str_detect(term, "^race_simplified|^country_region"))

print(results_subgroup)


#-----------------------
# 1. Load sensitivity models
#-----------------------
model_dir <- here("data", "processed", "sensitivity_timepoint")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
objects <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

# Keep only glmer models
models <- objects[sapply(objects, inherits, what = "glmerMod")]

#-----------------------
# 2. Extract fixed effects with CIs
#-----------------------
extract_model_results <- function(model, model_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(model = model_name)
}

fixed_effects <- map2_dfr(models, names(models), extract_model_results)

#-----------------------
# 3. Filter subgroup terms (race or country_region)
#-----------------------
results_long <- fixed_effects %>%
  filter(str_detect(term, "^(race_simplified|country_region)")) %>%
  select(model, term, estimate, conf.low, conf.high) %>%
  mutate(
    # Convert log-odds to OR
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    # Round
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    OR_CI = paste0(estimate, " (", conf.low, " - ", conf.high, ")"),
    category = case_when(
      str_detect(model, "crude_aerobic") ~ "Crude Aerobic (OR (95% CI))",
      str_detect(model, "adjusted_aerobic") ~ "Adjusted Aerobic (OR (95% CI))",
      str_detect(model, "crude_RT") ~ "Crude RT (OR (95% CI))",
      str_detect(model, "adjusted_RT") ~ "Adjusted RT (OR (95% CI))"
    )
  ) %>%
  select(term, category, OR_CI)

#-----------------------
# 4. Pivot wider for table
#-----------------------
results_clean <- results_long %>%
  group_by(term, category) %>%
  summarise(OR_CI = paste(OR_CI, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = OR_CI)

#-----------------------
# 5. Reorder and format
#-----------------------
desired_order <- c(
  "race_simplifiedBlack", "race_simplifiedOther",
  "country_regionAustralia", "country_regionNorth America", "country_regionEurope",
  "country_regionAfrica", "country_regionOther"
)

results_clean <- results_clean %>%
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
    term = str_remove(term, "^race_simplified|^country_region")
  ) %>%
  relocate(subgroup_type, .before = term) %>%
  rename(Subgroup = term, `Subgroup Type` = subgroup_type)

#-----------------------
# 6. Create flextable
#-----------------------
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

ft_sensitivity <- flextable(results_clean) %>%
  merge_v(j = "Subgroup Type") %>%
  autofit() %>%
  set_caption("Table X. Sensitivity Analysis (Month 30 removed): Mixed-effects logistic regression findings",
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

#-----------------------
# 7. Save to Word
#-----------------------
save_as_docx(
  ft_sensitivity,
  path = here("output", "supplemental", "sensitivity_analyses", "sensitivity_analysis_remove_M30.docx")
)