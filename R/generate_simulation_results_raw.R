generate_simulation_res_raw <- function(sim_summary){

  sim_summary %>%
    pmap_dfr(.f = function(n_success_it, path, ...){
      if(n_success_it == 0)        # Failed Simulation
        return(data.frame(NULL))

      ret <- list.files(path, full.names = TRUE) %>%
        grep(".rds", ., value = TRUE)%>%
        map_dfr(.f = function(.file){
          .file %>%
            read_rds() %>%
            unlist %>%
            t %>%
            data.frame()

        })

      # return object
      tibble(
        ...,                   # Copy over simulation parameters
        n_success_it,
        sim_pi_cns = ret %>% pull(p.cen) %>% list(),
        train = ret %>% select(starts_with("train_res")) %>%
          rename_all(str_sub, start=11)%>% list(),
        test = ret %>% select(starts_with("test_res")) %>%
          rename_all(str_sub, start=10)%>% list()
      )
    })
}
