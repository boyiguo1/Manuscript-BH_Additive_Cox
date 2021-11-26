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

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

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
