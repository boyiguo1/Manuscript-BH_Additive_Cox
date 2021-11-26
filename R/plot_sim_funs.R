plot_sim_funs <- function(){

  source("~/Manuscript-BH_Additive_Cox/Sim/Code/sim_funs.R")


  # Plot the functions
  list(f_1, f_2, f_3, f_4) %>%
    map(.f = function(fun){
      ggplot() +
        xlim(-4, 4) +
        geom_function(fun = fun) +
        theme(axis.title.y = element_blank())
    }) %>%
    ggarrange(plotlist = .,label.y = "Log Hazard Ratio")
}
