# --------------------------------------------------
# Master script for running all scripts from cleaning to analysis output 
# and supplemental materials.
# --------------------------------------------------

# Prepare environment
install.packages("renv")
library(renv)
renv::restore()

pacman::p_load(here, tidyverse)

# Define the list of script paths for each batch
cleaning_merging_scripts <- c(
  here("scripts", "01_demographics.R"),
  here("scripts", "01_eortc_processing.R"),
  here("scripts", "01_medical.R"),
  here("scripts", "01_off_study.R"),
  here("scripts", "01_PA_99th_percentile.R"),
  here("scripts", "02_data_merging.R")
)

analyses <- c(
  here("scripts", "05_primary_analyses_race_country.R"),
  here("scripts", "06_sensitivity_exclude_black_race_africa.R"),
  here("scripts", "06_sensitivity_stratify_disease_state.R"),
  here("scripts", "06_sensitivity_stratify_treatment_group.R"),
  here("scripts", "06_sensitivity_timepoint_at_least_2.R"),
  here("scripts", "06_sensitivity_timepoint_remove_M30.R")
)

output_supplemental <- c(
  here("scripts", "output_table_1_baseline_descriptives_stratified.R"),
  here("scripts", "supplemental_descriptive_PA_data.R"),
  here("scripts", "supplemental_figure_S1_activity_types.R"),
  here("scripts", "supplemental_table_descriptives_by_validity_completecases_timepoints.R")
)

# Function to run scripts from a given path list
run_scripts_batch <- function(script_paths, batch_name) {
  message("\n🔄 Starting batch: ", batch_name)
  
  # Loop over each script in the batch and run it
  invisible(lapply(script_paths, function(script_path) {
    if (file.exists(script_path)) {
      message("\n🔄 Loading and running: ", basename(script_path))
      source(script_path, echo = TRUE)
      message("✅ Finished: ", basename(script_path))
    } else {
      warning("⚠️ Script not found: ", script_path)
    }
  }))
  
  # Brief message after running the batch
  message("\n✅ Batch completed: ", batch_name)
  message("🔹 All scripts in the '", batch_name, "' batch have been successfully executed.\n")
}

# Run the batches in sequence
run_scripts_batch(cleaning_merging_scripts, "Data Cleaning and Merging")
run_scripts_batch(analyses, "Analyses")
run_scripts_batch(output_supplemental, "Supplemental Output")

message("\n🎉 All scripts have been run successfully.")