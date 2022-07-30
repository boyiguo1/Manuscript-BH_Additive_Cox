summarize_var_sel_scores <- function(dat){
  # browser()
  dat %>%
    map_dfr(
      .f = function(scn_dat){
        scn_dat %>%
          group_by(p, rho, pi_cns, method) %>%
          summarize(
            across(c(recall, precision, mcc), list(mean = mean, sd = sd), na.rm = TRUE)
          )
      }
    )
}
