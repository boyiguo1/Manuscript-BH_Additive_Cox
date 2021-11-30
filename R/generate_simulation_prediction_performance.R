generate_simulation_res_raw <- function(sim_summary){
  # browser()
  sim_summary %>%
    pmap_dfr(.f = function(n_success, path, ...){
      # browser()

      if(n_success == 0)        # Failed Simulation
        return(data.frame(NULL))

      ret <- list.files(path, full.names = TRUE) %>%
        grep(".rds", ., value = TRUE)%>%
        map_dfr(.f = function(.file){
          ret <-
            read_rds(.file) %>%
            `[[`(sec) %>%
            data.frame() %>%
            rownames_to_column("method")
        }) %>% list() %>%
        tibble(...,
               n_success,
               list_measures = .)
#
#       fls %>%
#         map_dfr(.f = function(.file){
#           #browser
#           it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
#           ret <- data.frame(
#             it = it,
#             read_rds(.file) %>%
#               `[[`(sec) %>%
#               data.frame() %>%
#               rownames_to_column("method")

            #%>%
            # data.frame %>%
            # rownames_to_column("method") %>%
            # select(method, measure) %>%
            # pivot_wider(names_from = method,
            #             values_from = measure,
            #             names_glue = "{method}_{.value}"
            #             )
          )
        })# %>%
        # arrange(it) %>%

    })



  # ret <- fls %>%
  #     map_dfr(.f = function(.file){
  #       it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
  #       ret <- data.frame(
  #         # it = it,
  #         read_rds(.file) %>%
  #           `[[`(sec) %>%
  #           data.frame() %>%
  #           rownames_to_column("method")

  #%>%
  # data.frame %>%
  # rownames_to_column("method") %>%
  # select(method, measure) %>%
  # pivot_wider(names_from = method,
  #             values_from = measure,
  #             names_glue = "{method}_{.value}"
  #             )
  # )
  #     }) %>%
  #     arrange(it) %>%
  #     # arrange(mdl, s0) %>%
  #     mutate(#s0 = factor(s0),
  #       p = sim.df$p)#  %>%
  #
  # }) %>%
  # mutate(method = factor(method, levels = c("bglm_t", "bglm_de", "blasso",
  #                                           "bglm_t_group", "bglm_de_group", "blasso_group",
  #                                           "bglm_spline_t", "bglm_spline_de", "blasso_spline",
  #                                           "cosso", "acosso", "mgcv", "SB_GAM"
  # )))
}
