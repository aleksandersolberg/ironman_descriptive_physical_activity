#--------------------------------------------------------------------------------
# Script: IM PA study - supplement figures
# Author: Aleksander Solberg
# Purpose:
#--------------------------------------------------------------------------------
# Load packages
pacman::p_load(tidyverse, here, gtsummary, flextable)

#Load data
data <- readRDS(here("data", "processed", "data.rds"))
data_filtered <- readRDS(here("data", "processed", "data_filtered.rds"))

# Remove NA's from disease_state
data_filtered <- data_filtered %>% 
  filter(!is.na(disease_state))
#----------------------------------------------------------------------------------
# Activity type figure
#----------------------------------------------------------------------------------
#Plot average time per activity type
#Subset out activity data
activity_duration <- data_filtered %>% 
  select(id, timepoint, walking, running, cycling, weight_training, raquet_sports, swimming, other_aerobic,
         low_intensity_PA, other_vig_PA, disease_state)

#pivot longer for plotting
activity_duration_long <- activity_duration %>% 
  pivot_longer(
    cols = c(walking, running, cycling, weight_training, raquet_sports, swimming, other_aerobic,
             low_intensity_PA, other_vig_PA),
    names_to = "activity",
    values_to = "time"
  )

# Generate summary statistics
activity_duration_summary <- activity_duration_long %>%
  filter(timepoint != "Baseline") %>%
  group_by(disease_state, timepoint, activity) %>%
  summarise(
    n = n(),
    mean_time = mean(time, na.rm = TRUE),
    sd_time = sd(time, na.rm = TRUE),
    se_time = sd_time / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_time - se_time,
    ymax = mean_time + se_time
  ) %>%
  group_by(disease_state, timepoint) %>%
  mutate(
    total_mean_time = sum(mean_time, na.rm = TRUE),
    percent_of_total = (mean_time / total_mean_time) * 100
  ) %>%
  ungroup()


# Sort activity levels by overall mean_time (størst nederst)
activity_order <- activity_duration_summary %>%
  group_by(activity) %>%
  summarise(mean_total = mean(mean_time, na.rm = TRUE)) %>%
  arrange(mean_total) %>%
  pull(activity)

activity_duration_summary$activity <- factor(activity_duration_summary$activity, levels = activity_order)

# Generate activity labels for plotting
activity_labels <- c(
  walking = "Walking",
  running = "Running",
  cycling = "Cycling",
  weight_training = "Resistance Training",
  raquet_sports = "Raquet Sports",
  swimming = "Swimming",
  other_aerobic = "Other Aerobic Activities",
  low_intensity_PA = "Other Low Intensity Activities",
  other_vig_PA = "Other Vigorous Activities"
)

# Plot
activity_types <- ggplot(activity_duration_summary, aes(x = activity, y = mean_time, fill = timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = pmax(mean_time - se_time, 0), ymax = mean_time + se_time),  
                position = position_dodge(width = 0.8),
                width = 0.4, size = 0.8, alpha = 0.8) +
  scale_x_discrete(labels = activity_labels) +  
  labs(
    x = "Activity Type",
    y = "Minutes per Week"
  ) +
  scale_fill_viridis_d() +
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_blank(),
    strip.text = element_text(size = 14, face = "bold", family = "Times New Roman")
  ) +
  facet_wrap(~ timepoint, scales = "free_x") +
  coord_flip()

activity_types_strat <- ggplot(activity_duration_summary,
                               aes(x = activity, y = mean_time, fill = timepoint)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = pmax(mean_time - se_time, 0), ymax = mean_time + se_time),
                position = position_dodge(width = 0.8),
                width = 0.4, size = 0.8, alpha = 0.8) +
  scale_x_discrete(labels = activity_labels) +
  labs(x = "Activity Type", y = "Minutes per Week") +
  scale_fill_viridis_d() +
  theme_bw(base_family = "Times New Roman") +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold", family = "Times New Roman")
  ) +
  facet_grid(disease_state ~ timepoint, scales = "free_x") +
  coord_flip()

#-----------------------------------------------------------------------------
# Save figures
#-----------------------------------------------------------------------------
ggsave(
  filename = here("output", "supplemental",  "PA_descriptives", "Supp_activity_types.png"),
  plot = activity_types,
  width = 8,       # inches
  height = 6,      # inches
  dpi = 300        # high resolution
)

#-----------------------------------------------------------------------------
# Save figures
#-----------------------------------------------------------------------------
ggsave(
  filename = here("output", "supplemental",  "PA_descriptives", "Supp_activity_types_strat.png"),
  plot = activity_types_strat,
  width = 8,       # inches
  height = 6,      # inches
  dpi = 300        # high resolution
)