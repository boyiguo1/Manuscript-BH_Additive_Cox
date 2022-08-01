summarize_var_sel_scores <- function(dat){
  # browser()
  dat %>%
    map_dfr(
      .f = function(scn_dat){
        # browser()
        scn_dat %>%
          group_by(p, rho, pi_cns, method) %>%
          summarize(
            across(c(recall, precision, mcc), list(mean = mean, sd = sd), na.rm = TRUE),
            across(c(recall, precision, mcc),
                   ~sprintf("%.3f (%.3f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE)))
          )
      }
    )
}
