generate_simulation_censoring_proportion <- function(sim_summary){


  sim_summary %>%
    pull(path) %>%
    map( .f = function(sim){

      fls <- list.files(sim, full.names = TRUE)

      if(length(fls) == 0)
        return(na_dbl)

      ret <- fls %>%
        map_dbl(.f = function(.file){
          ret <-
            read_rds(.file) %>%
            `[[`("p.cen")
        }) #%>%
      #list
      # browser()
    }) %>%
    tibble(
      sim_summary %>% select(-c(path, n_success)),
      cns_rate=.)


}
