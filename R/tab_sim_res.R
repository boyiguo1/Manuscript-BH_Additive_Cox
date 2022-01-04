tab_sim_res <- function(dat, sec = c("deviance", "Cindex")){
  # browser()

  if(length(sec)>1) sec <- sec[1]

  dat %>%
    select(starts_with("sim_prmt"), contains({{sec}})) %>%
    pivot_longer(
      cols = ends_with(c(".mean", ".sd")),
      names_to = c("method", "measure"),
      names_pattern = "([a-z]*)\\.[a-z]*\\.([a-z]*)",
      values_to = "value"
      ) %>%
    pivot_wider(
      names_from = "measure"
    ) %>%
    mutate(
      stat = sprintf("%.2f (%.2f)", mean, sd)
    ) %>%
    select(-mean, -sd) %>%
    pivot_wider(
      names_from = method,
      values_from = stat
    ) %>%
    rename_with(str_remove, .cols = starts_with("sim_prmt"), pattern = "sim_prmt.") %>%
    arrange(p, rho, pi_cns)
}
