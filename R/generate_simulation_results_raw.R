generate_simulation_res_raw <- function(sim_summary){

  sim_summary %>%
    pmap_dfr(.f = function(n_success, path, ...){

      if(n_success == 0)        # Failed Simulation
        return(data.frame(NULL))

      ret <- list.files(path, full.names = TRUE) %>%
        grep(".rds", ., value = TRUE)%>%
        map_dfr(.f = function(.file){
          # browser()
          .file %>%
            read_rds() %>%
            magrittr::extract(c("train_res", "test_res", "p.cen")) %>%
            unlist %>%
            t %>%
            data.frame()
        })

      # return object
      tibble(
        ...,                   # Copy over simulation parameters
        n_success,
        sim_pi_cns = ret %>% pull(p.cen) %>% list(),
        train = ret %>% select(starts_with("train_res")) %>% rename_all(str_sub, start=11)%>% list(),
        test = ret %>% select(starts_with("test_res")) %>% rename_all(str_sub, start=10)%>% list()
      )
    })
}


generate_simulation_val_sel_raw <- function(sim_summary){

  sim_summary %>%
    pmap(.f = function(n_success, path, ...){

      if(n_success == 0)        # Failed Simulation
        return(data.frame(NULL))

      ret <- list.files(path, full.names = TRUE) %>%
        grep(".rds", ., value = TRUE)%>%
        map_dfr(.f = function(.file){
          # browser()
          .file %>%
            read_rds() %>%
            pluck("var_slct") %>%
            unlist %>%
            t %>%
            data.frame()
        })
      # browser()
      # return object
      tibble(
        ...,                   # Copy over simulation parameters
        ret
        #   n_success,
        #   sim_pi_cns = ret %>% pull(p.cen) %>% list(),
        #   train = ret %>% select(starts_with("train_res")) %>% rename_all(str_sub, start=11)%>% list(),
        #   test = ret %>% select(starts_with("test_res")) %>% rename_all(str_sub, start=10)%>% list()
      )
    })
}

