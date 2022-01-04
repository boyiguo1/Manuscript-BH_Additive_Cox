viz_sim_res <- function(dat, sec = c("deviance", "Cindex")){
  browser()

  if(length(sec)>1) sec <- "deviance"
  dat %>%
    select(starts_with("sim_prmt"), contains({{sec}})) %>%
    pivot_longer(cols = contains({{sec}})) %>%
    filter(str_ends(name, ".mean")) %>%
    ggplot(#data = binom_total_dat,
      aes(x = name, y = value)) +
    geom_point() +
    # geom_violin() +
    # geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45))+
    # coord_flip() +
    facet_grid(cols = vars(sim_prmt.p),
               rows= vars(simprmt.pi_cns))
# })

ggarrange(plotlist = tmp, ncol = 1)

}
