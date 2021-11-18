# DO NOT CHANGE THIS SECTION
## Receive the simulation parameters from job file
## Evaluate the simulation parameters in the R global environment
## For the Toy Example
## It is equivalent to run
## n <- 100

args=(commandArgs(TRUE))

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}



# Library & Helper Functions ----------------------------------------------
## TODO: add libraries and helper functions
library(tidyverse)

## TODO: Replace path here
# source("/Change/To/Absolute/Path/Of/Your/Code")



# Data Generating Process -------------------------------------------------
## Use Array ID as random seed ID
it <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
set.seed(it)

## TODO: Replace with your simulation code




# Fit Models------------------------------------------------------------------
## TODO: Replace with your model code



# Save Simulation Results -------------------------------------------------
## TODO: Replace with your result saving code




job_name <- Sys.getenv('SLURM_JOB_NAME')
# Recommendation: to save the results in individual rds files
saveRDS(ret,
        paste0("/data/user/boyiguo1/bcam/Res/", job_name,"/it_",it,".rds"))
