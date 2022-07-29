generate_simulation_mdl_failed_rate <- function(raw_dat){
  # browser()

  raw_dat %>%
    select(starts_with("sim_prmt"), n_success, train) %>%
    pmap_dfr(.f = function(train, ...){
      # if(!ncol(msr_df))
      #   return(data.frame(NULL))
      browser()

      train %>%
        dplyr::summarize(
          across(.cols = ends_with(".deviance"),
                 .fns = ~sum(is.na(.)))
        ) %>%
        rename_all(~str_replace(., patter = "deviance", replace = "fail")) %>%
        tibble(..., .)
    })
}
