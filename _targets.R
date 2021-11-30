library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c("tidyverse", "knitr", "rmarkdown",
               "simsurv",
               "unglue")
)

## Load your packages, e.g. library(targets).
# source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(


  # Simulation --------------------------------------------------------------
  tar_files(
    sim_res_path,
    list.files("/data/user/boyiguo1/bcam/Res",
               full.names = TRUE)
  ),


  tar_target(
    sim_summary,
    generate_simulation_summary(sim_res_path)
  ),

  tar_target(
    sim_pred_measure_raw,
    generate_simulation_res_raw(sim_summary)
  ),


  tar_target(
    sim_cnr_prop,
    sim_pred_measure_raw %>%
      rowwise %>%
      mutate(mean_cns = mean(unlist(sim_pi_cns)), sd_cns = sd(unlist(sim_pi_cns))) %>%
      ungroup() %>%
      select(-c(sim_pi_cns, train, test))
  ),

  tar_target(
    sim_train_res,
    sim_pred_measure_raw %>%
      generate_sim_pred_measure("train")
  ),

  tar_target(
    sim_test_res,
    sim_pred_measure_raw %>%
      generate_sim_pred_measure("test")
  ),

# ,

# Manuscript --------------------------------------------------------------
#* Section Paths ####
tar_files(manu_path,
          setdiff(
            grep("*.Rmd|*.bib",
                 list.files("Manuscript", full.names = TRUE),
                 value = TRUE
            ),
            'Manuscript/00-main.Rmd'
          )
),

tar_render(manu, "Manuscript/00-main.Rmd",
           output_file = "CPH_AM.pdf")
)
