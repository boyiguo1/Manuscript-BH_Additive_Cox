library(dplyr)


# Simulation Parameters ---------------------------------------------------
sim_prmt <- expand.grid(
  n = c(500)
)





# Helper Function for Setting Up Job --------------------------------------
start.sim <- function(
  n
  # TODO: add sim_prmt column names as arguments
) {
  #Compose job name
  job.name <- paste("bcam_sim_n", n)

  # NOTE:
  ## Job name has to be unique for each of your simulation settings
  ## DO NOT USE GENERIC JOB NAME FOR CONVENIENCE
  job.flag <- paste0("--job-name=",job.name)

  err.flag <- paste0("--error=",job.name,".err")

  out.flag <- paste0("--output=",job.name,".out")

  # Pass simulation parameters to jobs using export flag
  ## TODO: add other sim_prmt in this flag if necessary
  arg.flag <- paste0("--export=n=", n)

  # Create Jobs
  system(
    paste("sbatch", job.flag, err.flag, out.flag, arg.flag,
          "~/Manuscript-BH_Additive_Cox/Sim/Code/bcam_sim.job.job")
  )
}





# Set up job for all simulation settings ----------------------------------
for(i in 1:NROW(sim_prmt)){
  do.call(start.sim, sim_prmt[i,])
}
