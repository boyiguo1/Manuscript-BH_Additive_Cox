library(tidyverse)
library(unglue)

scale_path <- list.files("/data/user/boyiguo1/bcam/scale", full.names = TRUE)
names(scale_path) <- str_remove(scale_path, "/data/user/boyiguo1/bcam/scale/")
scale_vec <- scale_path %>%
  map(.f = function(path, ...){
  # job_name <- str_remove(path, "/data/user/boyiguo1/bcam/scale/")
  it_files <- list.files(path, full.names = TRUE )
  if(length(it_files) == 0)        # Failed Simulation
    return(NA)

  ret <- list.files(path, full.names = TRUE) %>%
    grep(".rds", ., value = TRUE)%>%
    map_dbl(.f = function(.file){
      # browser()
      .file %>%
        read_rds()
    }) %>% median

  # names(ret) <- job_names
  # browser()s
  # return object
  return(ret)
})

saveRDS(scale_vec,
        "~/Manuscript-BH_Additive_Cox/Sim/Code/scale_vec.RDS")
