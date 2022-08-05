viz_sim_res <- function(dat, sec = c("deviance", "Cindex")){
  # browser()

  if(length(sec)>1) sec <- sec[1]
  dat %>%
    select(starts_with("sim_prmt"), contains({{sec}})) %>%
    pivot_longer(
      cols = ends_with(c(".mean", ".sd")),
      names_to = c("method", "measure"),
      names_pattern = "([a-z]*)\\.[a-zA-Z]*\\.([a-z]*)",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = "measure"
    ) %>%
    rename_with(.fn = str_remove, pattern = "sim_prmt.", .cols = starts_with("sim_prmt")) %>%
    # pivot_longer(cols = contains({{sec}})) %>%
    # mutate(name = str_remove(name, paste0(".",sec))) %>%
    # pivot_wider()
    # filter(str_ends(name, ".mean")) %>%
    mutate(
      method = factor(method,
                    levels = c("lasso","mgcv", "cosso", "acosso", "bacox", "bamlasso"),
                    labels = c("LASSO", "mgcv", "COSSO", "aCOSSO", "bacox", "BHAM")),
      rho = factor(rho)
    ) %>%
    ggplot(#data = binom_total_dat,
      aes(x = method, y = mean,
        color=rho)) +
    # geom_jitter(aes(color=factor(sim_prmt.rho)))+
    geom_point(aes(shape = rho),
               position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.5))+
  # geom_violin() +
  # geom_boxplot() +

    # coord_flip() +
    facet_grid(
      cols = vars(p),
      rows= vars(pi_cns)
    ) +
    # labs(title = paste0("Simulaiton Restuls (",sec, ")"),
         # caption = "Error bar means 1 standard deviation")+
    ylab(sec) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315),
          axis.title.x = element_blank(),
          legend.position="top",
          strip.background =element_rect(fill=NA)) +
    scale_color_discrete(name = unname(TeX("$rho = $"))) +
    scale_shape_discrete(name = unname(TeX("$rho = $")))


}
