library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse", "knitr", "rmarkdown"))

## Load your packages, e.g. library(targets).
# source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style



# Manuscript --------------------------------------------------------------

#* Section Paths ####

tar_target(bib_path,
           "Manuscript/bibfile.bib",
           format = "file"),

tar_target(intro_path,
           "Manuscript/01-intro.Rmd",
           format = "file"),

tar_target(method_path,
           "Manuscript/02-method.Rmd",
           format = "file"),

tar_target(sim_path,
           "Manuscript/03-simulation.Rmd",
           format = "file"),

tar_target(real_data_path,
           "Manuscript/04-real_data.Rmd",
           format = "file"),

tar_target(disc_path,
           "Manuscript/05-conclusion.Rmd",
           format = "file"),

tar_render(manu, "Manuscript/00-main.Rmd",
           output_file = "CPH_AM.pdf")
)
