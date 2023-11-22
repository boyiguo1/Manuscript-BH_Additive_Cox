# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(unglue)

source(here("R/generate_simulation_summary.R"))

# STab: Simulation Scenarios + Iterations ----
sim_setting_df <- here("sim_data/") |>
  list.files(full.names = TRUE) |>
  generate_simulation_summary()

# TODO: save in a latex table/excel file.


# Curate Simulation Prediction Res Data ----
source(here("R/generate_simulation_results_raw.R"))
sim_pred_df <- sim_setting_df |>
  generate_simulation_res_raw()
