plot_true_sig <- function(){

  # f_1 <- function(x) (x*5-2)^2/20-3
  # f_2 <- function(x) exp(x+1)/10
  # f_3 <- function(x) 2*sin(x*4)
  # f_4 <- function(x) (1.4*x+0.5)/2

  plot_list <- list(f_1, f_2, f_3, f_4) %>%
    map(.f = function(sim_fun){
      x <- seq(from = -2, to = 2, by = 0.1)
      y <- sim_fun(x)
      ggplot() +
        xlim(-2, 2) +
        ylab("f(x)") +
        geom_function(fun = sim_fun) +
        geom_smooth(aes(x,y), method = "lm",
                    formula = y~x, se = FALSE, linetype = 4) +
        theme_pubr()
    })

  ggarrange(plotlist = plot_list,
            labels = 1:4)

}
