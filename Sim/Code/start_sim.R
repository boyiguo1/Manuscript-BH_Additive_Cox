library(dplyr)


# Simulation Parameters ---------------------------------------------------
sim_prmt <- expand.grid(
  n_train = c(500),
  n_test = c(1000),
  p = c(4, 10, 50, 100, 200),        # Number of Predictors
  rho = c(0, 0.5),                   # X Cov Structure AR(rho)
  pi_cns = c(0.15, 0.3, 0.45)        # Proportional of Censoring
)





# Helper Function for Setting Up Job --------------------------------------
start.sim <- function(
  n_train, n_test,
  p, rho,
  pi_cns
) {
  #Compose job name
  job.name <- paste0("bcam_sim_p=", p)

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
