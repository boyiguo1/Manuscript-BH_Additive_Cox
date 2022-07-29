make_sim_var_metric_raw <- function(dat){
  dat %>%
    map(.f = function(df){
      browser()

      # Simulation Meta Information
      p <- df$sim_prmt.p %>% first()
      rho <- df$sim_prmt.rho %>% first()
      pi_cns <- df$sim_prmt.pi_cns %>% first()

      # Create the vector for the truth
      truth <- rep(FALSE, p)
      truth[1:4] <- TRUE
      names(truth) <- paste0("X", 1:p)

      tmp_df <- df %>%
        select(-starts_with("sim_prmt.")) %>%
        mutate(
        tmp_index = 1:n()
      ) %>%
        pivot_longer(
          cols = !tmp_index,
          names_to = c("method", "variable"),
          names_patter= "(.*).(X.*)"#,
          # values_to = "inclusion",
          # names_sep = "."
        ) %>%
        pivot_wider(names_from = variable) %>%
        rowwise() %>%
        summarize(
          tmp_index, method,
          var_sel = list(c_across(starts_with("X")))#,
          # recall =

        )
    })
}
