library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c("tidyverse", "knitr", "rmarkdown",
               "simsurv")
)

## Load your packages, e.g. library(targets).
# source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)
source("./Sim/Code/helper_functions.R")
# lapply(list.files("./Sim/Code/", full.names = TRUE, recursive = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # target = function_to_make(arg), ## drake style

  # tar_target(target2, function_to_make2(arg)) ## targets style


  # Simulation --------------------------------------------------------------

  # tar_target(cns_pi, c(0.15, 0.3, 0.45)),
  # tar_target(sim_it, 1:10),

  # ** Examine Find Censoring Rate Function ---------------------------------
  # tar_target(cns_rate_batch, run_cns_rate_test(cns_pi), pattern = cross(cns_pi, map(sim_it))),




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
