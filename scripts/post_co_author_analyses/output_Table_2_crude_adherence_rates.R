
#-------------------------------------------------
# PA guideline adherence: overall + subgroups (split outputs)
#-------------------------------------------------
pacman::p_load(tidyverse, here, flextable, officer, scales, rlang)

# Load data
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))

#--------------------------------------------------------------------------------
# Prepare data: filter away Baseline, ensure numeric adherence, long format
#--------------------------------------------------------------------------------
data_long <- data_filtered %>%
  filter(timepoint != "Baseline") %>%
  mutate(
    aerobic_PA_guidelines = as.numeric(aerobic_PA_guidelines),
    RT_guidelines_30      = as.numeric(RT_guidelines_30)
  ) %>%
  mutate(timepoint = droplevels(factor(timepoint))) %>%
  select(timepoint, aerobic_PA_guidelines, RT_guidelines_30,
         race_simplified, country_region, disease_state, treatment_group_Baseline) %>%
  pivot_longer(
    cols      = c(aerobic_PA_guidelines, RT_guidelines_30),
    names_to  = "guideline",
    values_to = "adherence"
  ) %>%
  mutate(
    guideline = recode(guideline,
                       "aerobic_PA_guidelines" = "Aerobic PA Guidelines",
                       "RT_guidelines_30"      = "Resistance Training Guidelines"
    ),
    guideline = factor(guideline,
                       levels = c("Aerobic PA Guidelines", "Resistance Training Guidelines")
    )
  ) %>%
  filter(!is.na(adherence))

#================================================================================
# A) OVERALL ADHERENCE (no subgroup stratification)
#================================================================================
overall_tbl <- data_long %>%
  group_by(guideline, timepoint) %>%
  summarise(
    N          = n(),
    Adherent   = sum(adherence),
    Proportion = mean(adherence),
    .groups    = "drop"
  ) %>%
  mutate(
    Adherence = paste0(Adherent, " / ", N, " (", round(100 * Proportion), "%)")
  ) %>%
  select(guideline, timepoint, Adherence) %>%
  pivot_wider(names_from = timepoint, values_from = Adherence) %>%
  arrange(guideline)

# Export overall to Word
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

overall_ft <- overall_tbl %>%
  flextable() %>%
  autofit() %>%
  bold(part = "header") %>%
  set_caption("Table: Overall Adherence to PA Guidelines by Timepoint",
              align_with_table = FALSE)

save_as_docx(overall_ft,
             path = here("output", "Table_overall_PA_adherence.docx")
)

#================================================================================
# B) SUBGROUP ADHERENCE (Race, Country Region, Disease State, Treatment)
#================================================================================
subgroups <- c("race_simplified", "country_region", "disease_state", "treatment_group_Baseline")

subgroup_labels <- tribble(
  ~Variable,                  ~`Subgroup Type`,
  "race_simplified",          "Race",
  "country_region",           "Country Region",
  "disease_state",            "Disease State",
  "treatment_group_Baseline", "First On-Study Treatment Regimen"
)

subgroup_tbl <- map_dfr(subgroups, function(sb) {
  data_long %>%
    filter(!is.na(.data[[sb]])) %>%
    group_by(guideline, timepoint, .data[[sb]]) %>%
    summarise(
      N          = n(),
      Adherent   = sum(adherence),
      Proportion = mean(adherence),
      .groups    = "drop"
    ) %>%
    rename(Subgroup = !!sym(sb)) %>%
    mutate(Variable = sb)
}) %>%
  # Attach display label for subgroup type
  left_join(subgroup_labels, by = "Variable") %>%
  # Standardize factor ordering
  mutate(
    Subgroup = case_when(
      Variable == "disease_state" ~
        factor(as.character(Subgroup), levels = c("mHSPC", "CRPC")),
      Variable == "treatment_group_Baseline" ~
        factor(as.character(Subgroup), levels = c(
          "ADT + ARPI",
          "ADT + ARPI + chemo",
          "ADT + chemo",
          "ADT monotherapy"
        )),
      TRUE ~ as.factor(Subgroup)
    ),
    `Subgroup Type` = factor(
      `Subgroup Type`,
      levels = c("Race", "Country Region", "Disease State", "First On-Study Treatment Regimen")
    ),
    Adherence = paste0(Adherent, " / ", N, " (", round(100 * Proportion), "%)")
  ) %>%
  select(guideline, `Subgroup Type`, Subgroup, timepoint, Adherence) %>%
  pivot_wider(names_from = timepoint, values_from = Adherence) %>%
  arrange(guideline, `Subgroup Type`, Subgroup)

# Export subgroup to Word
subgroup_ft <- subgroup_tbl %>%
  flextable() %>%
  autofit() %>%
  bold(part = "header") %>%
  set_caption("Table: Adherence to PA Guidelines by Subgroup and Timepoint",
              align_with_table = FALSE)

save_as_docx(subgroup_ft,
             path = here("output", "Table_subgroup_PA_adherence.docx")
)
