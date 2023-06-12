make_sim_var_sel_tex <- function(dat){
  browser()
  dat %>%
    pivot_longer(
      cols = !c(p, rho, pi_cns, method),
      names_to = c("metric","stat"),
      names_sep = "_"
    ) %>%
    pivot_wider(
      names_from = stat,
      values_from = value
    ) %>%
    mutate(
      output = sprintf(

      )
    )
}
