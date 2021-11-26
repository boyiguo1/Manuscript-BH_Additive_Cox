generate_simulation_summary <- function(path){
  # browser()
  map_dfr(path, .f = function(sim){
    # sim <- sims[2]
    sim.df <- unglue_data(sim,
                          "{}/bcam_sim_p={p},rho={rho},pi_cns={pi_cns}") %>%
      mutate( p = as.numeric(p))
    fls <- list.files(sim, full.names = TRUE)
    fls <- fls[grep(".rds", x=fls)]
    n <- length(fls)

    data.frame(sim.df, n_success = n, path = sim)

  }) %>%
    arrange(p)
}
