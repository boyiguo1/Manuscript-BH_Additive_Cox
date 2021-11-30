generate_simulation_summary <- function(path){
  # browser()

  # Tmp helper for renaming simulation parameters
  paste_sim_prmt_front <- function(vec){
    paste0("sim_prmt.", vec)
  }


  map_dfr(path, .f = function(sim){
    # sim <- sims[2]
    sim.df <- unglue_data(sim,
                          "{}/bcam_sim_p={p},rho={rho},pi_cns={pi_cns}") %>%
      mutate_all(as.numeric) %>%
      rename_all(paste_sim_prmt_front)

    fls <- list.files(sim, full.names = TRUE)
    fls <- fls[grep(".rds", x=fls)]
    n <- length(fls)

    data.frame(sim.df, n_success = n, path = sim)

  })
}
