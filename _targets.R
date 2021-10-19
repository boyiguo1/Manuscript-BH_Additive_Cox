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

)
