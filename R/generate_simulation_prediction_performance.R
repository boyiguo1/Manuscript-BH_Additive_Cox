generate_sim_pred_measure <- function(raw_dat, sec){
  # browser()
  raw_dat %>%
    rename(msr = {{sec}}) %>%
    select(starts_with("sim_prmt"), msr) %>%
    pmap_dfr(.f = function(msr_df, ...){
      # if(!ncol(msr_df))
      #   return(data.frame(NULL))
      # browser()

      msr_df %>%
        dplyr::summarize(
          across(.fns = list(mean = mean, sd = sd),
                 .names = "{.col}.{.fn}")
        ) %>%
        tibble(...,
               .)
    })

}
