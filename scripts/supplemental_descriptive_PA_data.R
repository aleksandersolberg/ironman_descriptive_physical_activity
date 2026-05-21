#================================================================================
# Script: IM PA study - descriptives
# Author: Aleksander Solberg
# Purpose: Script for creating descriptive data and figures for PA levels.
#================================================================================

# Load packages
pacman::p_load(tidyverse, here, patchwork, flextable, scales, gtsummary)

# Load data
data <- readRDS(here("data", "processed", "data.rds"))
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))

#--------------------------------------------------------------------------------
# Prepare data for summary tables
#--------------------------------------------------------------------------------
# Subset out relevant PA variables
PA_vars <- data_filtered %>%
  select(id, timepoint, MVPA_minutes_week, moderate_minutes_week, vigorous_minutes_week, aerobic_MVPA_minutes_week, weight_training)

# Pivot longer to a long format
PA_long <- PA_vars %>%
  pivot_longer(
    cols = c(MVPA_minutes_week, aerobic_MVPA_minutes_week, moderate_minutes_week, vigorous_minutes_week, weight_training),
    names_to = "variable",
    values_to = "value"
  )

PA_summary <- PA_long %>%
  filter(timepoint != "Baseline") %>%
  group_by(timepoint, variable) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    variable = recode(variable,
                      "moderate_minutes_week" = "Moderate PA (min/week)",
                      "vigorous_minutes_week" = "Vigorous PA (min/week)",
                      "aerobic_MVPA_minutes_week" = "Aerobic MVPA (min/week)",
                      "weight_training" = "Resistance Training (min/week)"
    ),
    value = ifelse(!is.na(median), sprintf("%.0f (%.0f)", median, IQR), NA)
  )

PA_summary_wide <- PA_summary %>%
  select(timepoint, variable, value, n) %>%
  pivot_wider(
    names_from = variable,
    values_from = c(value, n),
    names_glue = "{variable}_{.value}"
  ) %>%
  distinct(timepoint, .keep_all = TRUE)

#--------------------------------------------------------------------------------
# Resistance training summary
#--------------------------------------------------------------------------------

resistance_training <- data_filtered %>%
  select(id, timepoint, weight_training) %>%
  mutate(any_RT = weight_training > 0)

resistance_summary <- resistance_training %>%
  filter(timepoint != "Baseline") %>%
  group_by(timepoint) %>%
  summarise(
    n_did_RT = sum(any_RT, na.rm = TRUE),
    total_any_RT = sum(!is.na(weight_training)),
    prop_did_RT = n_did_RT / total_any_RT,
    .groups = "drop"
  ) %>%
  mutate(n_percent_RT = sprintf("%d (%.1f%%)", n_did_RT, prop_did_RT * 100)) %>%
  select(timepoint, total_any_RT, n_percent_RT)

#--------------------------------------------------------------------------------
# Guideline adherence summary
#--------------------------------------------------------------------------------

PA_guidelines_vars <- data_filtered %>%
  select(id, timepoint, aerobic_PA_guidelines, RT_guidelines_30, joint_PA_guidelines, aerobic_MPA_guidelines, aerobic_VPA_guidelines)

PA_guidelines_summary <- PA_guidelines_vars %>%
  filter(timepoint != "Baseline") %>%
  mutate(
    aerobic_only = aerobic_PA_guidelines == TRUE & RT_guidelines_30 == FALSE,
    RT_only = aerobic_PA_guidelines == FALSE & RT_guidelines_30 == TRUE,
    no_guidelines = aerobic_PA_guidelines == FALSE & RT_guidelines_30 == FALSE
  ) %>%
  group_by(timepoint) %>%
  summarise(
    n_MPA_aerobic = sum(aerobic_MPA_guidelines, na.rm = TRUE),
    n_VPA_aerobic = sum(aerobic_VPA_guidelines, na.rm = TRUE),
    n_aerobic = sum(aerobic_PA_guidelines, na.rm = TRUE),
    n_RT = sum(RT_guidelines_30, na.rm = TRUE),
    n_joint = sum(joint_PA_guidelines, na.rm = TRUE),
    n_aerobic_only = sum(aerobic_only, na.rm = TRUE),
    n_RT_only = sum(RT_only, na.rm = TRUE),
    n_none = sum(no_guidelines, na.rm = TRUE),
    total_MPA_aerobic = sum(!is.na(aerobic_MPA_guidelines)),
    total_VPA_aerobic = sum(!is.na(aerobic_VPA_guidelines)),
    total_aerobic = sum(!is.na(aerobic_PA_guidelines)),
    total_RT = sum(!is.na(RT_guidelines_30)),
    total_joint = sum(!is.na(joint_PA_guidelines)),
    total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    prop_MPA_aerobic = n_MPA_aerobic / total_MPA_aerobic,
    prop_VPA_aerobic = n_VPA_aerobic / total_VPA_aerobic,
    prop_aerobic = n_aerobic / total_aerobic,
    prop_RT = n_RT / total_RT,
    prop_joint = n_joint / total_joint,
    prop_aerobic_only = n_aerobic_only / total,
    prop_RT_only = n_RT_only / total,
    prop_none = n_none / total,
    MPA_aerobic_combined = sprintf("%d (%.1f%%)", n_MPA_aerobic, prop_MPA_aerobic * 100),
    VPA_aerobic_combined = sprintf("%d (%.1f%%)", n_VPA_aerobic, prop_VPA_aerobic * 100),
    aerobic_combined = sprintf("%d (%.1f%%)", n_aerobic, prop_aerobic * 100),
    RT_combined = sprintf("%d (%.1f%%)", n_RT, prop_RT * 100),
    joint_combined = sprintf("%d (%.1f%%)", n_joint, prop_joint * 100),
    aerobic_only_combined = sprintf("%d (%.1f%%)", n_aerobic_only, prop_aerobic_only * 100),
    RT_only_combined = sprintf("%d (%.1f%%)", n_RT_only, prop_RT_only * 100),
    none_combined = sprintf("%d (%.1f%%)", n_none, prop_none * 100)
  )

#--------------------------------------------------------------------------------
# Merge all tables together
#--------------------------------------------------------------------------------

combined_PA_summary <- PA_summary_wide %>%
  left_join(PA_guidelines_summary, by = "timepoint") %>%
  left_join(resistance_summary, by = "timepoint")

#--------------------------------------------------------------------------------
# Create Table 2a: Continuous PA Variables
#--------------------------------------------------------------------------------
continuous_vars <- combined_PA_summary %>%
  select(
    timepoint,
    `Moderate PA (min/week)_value`,
    `Vigorous PA (min/week)_value`,
    `Aerobic MVPA (min/week)_value`,
    `Resistance Training (min/week)_value`
  )

set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  border.color = "black",
  border.style = "solid",
  padding = 2
)

flextable(continuous_vars) %>%
  set_header_labels(
    timepoint = "Timepoint",
    `Moderate PA (min/week)_value` = "MPA (min/week)",
    `Vigorous PA (min/week)_value` = "VPA (min/week)",
    `Aerobic MVPA (min/week)_value` = "Aerobic MVPA (min/week)",
    `Resistance Training (min/week)_value` = "Resistance Training (min/week)"
  ) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table 2a. Continuous PA Variables", align_with_table = FALSE) %>%
  save_as_docx(path = here("output", "supplemental", "PA_descriptives", "Supp_PA_continuous.docx"))

#--------------------------------------------------------------------------------
# Create Table 2b: Guideline Adherence
#--------------------------------------------------------------------------------
guideline_vars <- combined_PA_summary %>%
  select(
    timepoint,
    total_any_RT,
    n_percent_RT,
    total_MPA_aerobic,
    MPA_aerobic_combined,
    total_VPA_aerobic,
    VPA_aerobic_combined,
    total_aerobic,
    aerobic_combined,
    total_RT,
    RT_combined,
    total_joint,
    joint_combined,
    aerobic_only_combined,
    RT_only_combined,
    none_combined
  )

flextable(guideline_vars) %>%
  set_header_labels(
    timepoint = "Timepoint",
    total_any_RT = "n",
    n_percent_RT = "Reported any RT (n, %)",
    total_MPA_aerobic = "n",
    MPA_aerobic_combined = "Moderate PA Aerobic Guideline Adherence",
    total_VPA_aerobic = "n",
    VPA_aerobic_combined = "Vigorous PA Aerobic Guideline Adherence",
    total_aerobic = "n",
    aerobic_combined = "Aerobic Guideline Adherence (n, %)",
    total_RT = "n",
    RT_combined = "RT Guideline Adherence (n, %)",
    total_joint = "n",
    joint_combined = "Combined Guideline Adherence (n, %)",
    aerobic_only_combined = "Only aerobic PA guidelines",
    RT_only_combined = "Only RT guidelines",
    none_combined = "Met no guidelines"
  ) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table 2b. Guideline Adherence", align_with_table = FALSE) %>%
  save_as_docx(path = here("output", "supplemental", "PA_descriptives",  "Supp_PA_guidelines.docx"))

#-------------------------------------------------------------------------------
#calculate median and mean duration of RT among those who did RT (supplemental analysis)
#-------------------------------------------------------------------------------
resistance_training_filtered <- resistance_training %>% 
  filter(timepoint != "Baseline") %>%
  filter(any_RT == TRUE) %>% 
  group_by(timepoint) %>% 
  summarise(
    RT_dur_mean = mean(weight_training, na.rm = TRUE),
    RT_dur_sd = sd(weight_training, na.rm = TRUE),
    RT_dur_median = median(weight_training, na.rm = TRUE),
    RT_dur_iqr = IQR(weight_training, na.rm = TRUE)
  ) %>% 
  mutate(
    RT_dur_mean_sd = sprintf("%.2f (%.2f)", RT_dur_mean, RT_dur_sd)
  ) %>% 
  select(timepoint, RT_dur_mean_sd)

resistance_training_filtered

#Generate table showing mean RT among those who reported any RT
flextable(resistance_training_filtered) %>%
  set_header_labels(
    timepoint = "Timepoint",
    `RT_dur_mean_sd` = "Resistance Training (min/week)") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table Sx. RT duration among those reporting any RT", align_with_table = FALSE) %>%
  save_as_docx(path = here("output", "supplemental", "PA_descriptives", "Supp_RT_dur_any_RT.docx"))



#------------------------------------------------------
# ONLY FOR ORAL PRESENTATION
#-----------------------------------------------------
# Generate descriptive figure for crude adherence rates for presentation

# 0) Rens guideline-navn for robust filtrering (case-insensitiv + trim)
df <- adherence_table %>%
  mutate(
    guideline = case_when(
      str_detect(str_to_lower(str_trim(guideline)), "aerobic")    ~ "Aerobic PA guidelines",
      str_detect(str_to_lower(str_trim(guideline)), "resistance") ~ "Resistance Training Guidelines",
      TRUE ~ guideline
    )
  )

# 1) Riktig rekkefølge på timepoints
df <- df %>%
  mutate(
    timepoint = factor(timepoint, levels = c("Month 6", "Month 18", "Month 30"))
  )

# Eksplisitte nivåer (slik du satte dem)
race_levels   <- c("White","Black","Other")
region_levels <- c("Europe", "North America", "Australia", "Africa", "Other")

# 2) Sett Subgroup-rekkefølge per Variable (eller fallback sortering etter median Proportion)
df <- df %>%
  group_by(Variable) %>%
  mutate(
    Subgroup = case_when(
      Variable == "race_simplified" & !is.null(race_levels)   ~ factor(Subgroup, levels = race_levels),
      Variable == "country_region"  & !is.null(region_levels) ~ factor(Subgroup, levels = region_levels),
      TRUE ~ fct_reorder(Subgroup, Proportion, .fun = median, .desc = TRUE) # fallback: sorter etter median-rate
    )
  ) %>%
  ungroup()

# Fargepalett: dynamisk
palette_fn <- function(n) {
  hues <- scales::hue_pal()(max(n, 6))
  hues[seq_len(n)]
}

# Hjelpefunksjon: lag og lagre plott per kombinasjon av guideline og Variable
plot_gxv <- function(g, var_name) {
  d <- df %>%
    filter(guideline == g, Variable == var_name) %>%
    arrange(timepoint, Subgroup)
  
  if (nrow(d) == 0) {
    message(sprintf("Ingen rader for guideline='%s' og Variable='%s'. Sjekk stavemåte/innhold.", g, var_name))
    return(invisible(NULL))
  }
  
  # Unike Subgroup-nivåer
  subgroups_here <- levels(d$Subgroup)
  if (is.null(subgroups_here)) subgroups_here <- unique(d$Subgroup)
  pal <- setNames(palette_fn(length(subgroups_here)), subgroups_here)
  
  p <- ggplot(d, aes(x = timepoint, y = Proportion, fill = Subgroup)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "grey25", na.rm = TRUE) +
    # ❌ Fjernet prosent-etiketter på stolpene (tidligere geom_text med percent())
    # ✅ Beholder N-etiketter (om du vil fjerne dem også, kommenter ut blokken under)
    geom_text(
      aes(label = paste0("N=", N), group = Subgroup),
      position = position_dodge(width = 0.85),
      vjust = 1.25, size = 3.1, color = "grey30", na.rm = TRUE
    ) +
    # ❌ Fjernet percent_format på y-aksen; viser rå proporsjon [0–1]
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      limits = c(0, 1.05),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = pal) +
    labs(
      x = "Timepoint",
      y = "Adherence (proportion)",
      fill = "Subgroup"
    ) +
    # ✅ Hvit bakgrunn uten grid-linjer
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 18)
    )
  
  print(p)
  
  # Lagre PNG
  file_stub <- paste(
    "adherence",
    str_replace_all(tolower(g), "[^a-z0-9]+", "_"),
    var_name,
    sep = "_"
  )
  ggsave(
    file.path(here("output", "presentations"), paste0(file_stub, ".png")),
    plot = p, width = 10, height = 6.2, dpi = 300)
}

# Kjør: 2 guidelines × 2 subgroup-variabler → 4 figurer
plot_gxv("Aerobic PA guidelines", "race_simplified")
plot_gxv("Aerobic PA guidelines", "country_region")
plot_gxv("Resistance Training Guidelines", "race_simplified")
plot_gxv("Resistance Training Guidelines", "country_region")


plot_gxv("Aerobic PA guidelines", "disease_state_baseline")
plot_gxv("Resistance Training Guidelines", "disease_state_baseline")
plot_gxv("Aerobic PA guidelines", "treatment_group_Baseline")
plot_gxv("Resistance Training Guidelines", "treatment_group_Baseline")
