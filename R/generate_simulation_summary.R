#' Generate the success rate for each simulation setting
#'
#' @param path a vector of paths containing the folder path containing all
#'  individual iterations under the same simulaiton scenario
#'
#' @return a data.frame containing the simulaiton setting parameters and the
#' success rate for each simulation setting
#' @export

generate_simulation_summary <- function(path){

  # Tmp helper for renaming simulation parameters
  paste_sim_prmt_front <- function(vec){
    paste0("sim_prmt.", vec)
  }


  map_dfr(path, .f = function(sim){
    sim.df <- unglue_data(
      sim,
      "{}/bcam_sim_p={p},rho={rho},pi_cns={pi_cns}"
    ) %>%
      mutate_all(as.numeric) %>%
      rename_all(paste_sim_prmt_front)

    fls <- list.files(sim, full.names = TRUE)
    fls <- fls[grep(".rds", x=fls)]
    n <- length(fls)

    data.frame(sim.df, n_success_it = n, path = sim)

  })
}
