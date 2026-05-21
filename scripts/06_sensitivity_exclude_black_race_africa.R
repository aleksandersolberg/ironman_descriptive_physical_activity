#===============================================================================
# Script: IM PA study - sensitivity analyses - remove Black participants from Africa
# Author: Aleksander Solberg
#===============================================================================
#-----------------------
# 1. Prepare environment
#------------------------
pacman::p_load(tidyverse, here, lme4, emmeans, broom.mixed, flextable, gtsummary)

# Load analysis dataset
data <- readRDS(here("data", "processed", "analysis_data.rds"))

# Filter out Black participants from the Africa Country Region
data_race_filtered <- data %>%
  filter(!(race_simplified == "Black" & country_region == "Africa"))

#-----------------------------------
# Repeat analyses for race without Black participants from Africa
#-----------------------------------

# Aerobic PA guidelines
crude_aerobic_race_filtered <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                       (1 | id) + (1 | site),
                                     data = data_race_filtered, family = binomial,
                                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Adjusted analyses
adjusted_aerobic_race_filtered <- glmer(aerobic_PA_guidelines ~ race_simplified + timepoint +
                                          disease_state_baseline + age_subgroup_baseline +
                                          education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                                          (1 | id) + (1 | site),
                                        data = data_race_filtered, family = binomial,
                                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# RT guidelines
crude_RT_race_filtered <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                  (1 | id) + (1 | site),
                                data = data_race_filtered, family = binomial,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

adjusted_RT_race_filtered <- glmer(RT_guidelines_30 ~ race_simplified + timepoint +
                                     disease_state_baseline + age_subgroup_baseline +
                                     education_binary_baseline + ecog_binary_baseline + treatment_group_baseline +
                                     (1 | id) + (1 | site),
                                   data = data_race_filtered, family = binomial,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#-----------------------
# 2. Save outputs as RDS
#------------------------
saveRDS(crude_aerobic_race_filtered, here("data", "processed", "sensitivity_race", "crude_aerobic_race_filtered.rds"))
saveRDS(adjusted_aerobic_race_filtered, here("data", "processed", "sensitivity_race", "adjusted_aerobic_race_filtered.rds"))
saveRDS(crude_RT_race_filtered, here("data", "processed", "sensitivity_race", "crude_RT_race_filtered.rds"))
saveRDS(adjusted_RT_race_filtered, here("data", "processed", "sensitivity_race", "adjusted_RT_race_filtered.rds"))

#-----------------------------------
# Extract and pool results
#-----------------------------------
model_dir <- here("data", "processed", "sensitivity_race")
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
models <- setNames(lapply(model_files, readRDS), tools::file_path_sans_ext(basename(model_files)))

# Function to extract OR, CI, and p-values
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

# Apply to all models
results <- map2_dfr(models, names(models), extract_or_ci)

# Filter only race terms
results_race <- results %>%
  filter(str_detect(term, "^race_simplified"))

#-----------------------------------
# Create clean table for race
#-----------------------------------

final_results_clean <- results_race %>%
  mutate(
    Subgroup = str_remove(term, "^race_simplified"),
    category = case_when(
      str_detect(model, "crude_aerobic") ~ "Crude Aerobic",
      str_detect(model, "adjusted_aerobic") ~ "Adjusted Aerobic",
      str_detect(model, "crude_RT") ~ "Crude RT",
      str_detect(model, "adjusted_RT") ~ "Adjusted RT"
    ),
    Subgroup_Type = "Race (ref = White)"  
  ) %>%
  select(Subgroup_Type, Subgroup, category, OR_CI) %>%
  pivot_wider(names_from = category, values_from = OR_CI) %>%
  # Reorder columns
  select(Subgroup_Type, Subgroup, `Crude Aerobic`, `Adjusted Aerobic`, `Crude RT`, `Adjusted RT`)


#-----------------------------------
# Create flextable
#-----------------------------------
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

ft_race <- flextable(final_results_clean) %>%
  merge_v(j = "Subgroup_Type") %>%  # ✅ Merge subgroup type cells vertically
  autofit() %>%
  set_caption("Sensitivity Analysis: Race (excluding Black participants from Africa)",
              align_with_table = FALSE) %>%
  delete_part(part = "header") %>%
  add_header_row(
    values = c("Subgroup Type", "Subgroup", "Aerobic Guidelines", "Resistance Training Guidelines"),
    colwidths = c(1, 1, 2, 2)
  ) %>%
  add_header_row(
    values = c("", "", "cOR (95% CI)", "aOR (95% CI)", "cOR (95% CI)", "aOR (95% CI)"),
    colwidths = c(1, 1, 1, 1, 1, 1)
  ) %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  hline_top(part = "header")

  # Preview
ft_race

# Save to Word
save_as_docx(
  ft_race,
  path = here("output", "supplemental", "sensitivity_analyses", "Table_sensitivity_race.docx"))