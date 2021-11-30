generate_simulation_censoring_proportion <- function(sim_summary){

  sim_summary %>%
    pmap_dfr(.f = function(n_success, path, ...){

      if(n_success == 0)
        return(na_dbl)
      # browser()
      ret <- list.files(path, full.names = TRUE) %>%
        grep(".rds", ., value = TRUE)%>%
        map_dbl(.f = function(.file){
          ret <-
            read_rds(.file) %>%
            `[[`("p.cen")
        }) %>% list() %>%
        tibble(...,
               n_success,
               p_censor = .)
    })
}
