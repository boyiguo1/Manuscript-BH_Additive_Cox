library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c(
    "simsurv", # Simulation packages
    "tidyverse", "ggplot2", "unglue", # Data wrangling packages
    "knitr", "rmarkdown", "rticles", "xtable", # Manuscript packages
    "gtsummary","survival", "ggpubr", "survminer",
    "glmnet", "BHAM", "BhGLM" # Data analysis packages
  ),
  imports = c("BHAM")
)


## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

tar_plan(
  # Simulation --------------------------------------------------------------
  # tar_files(
  #   sim_res_path,
  #   list.files("/data/user/boyiguo1/bcam/Res",
  #              full.names = TRUE)
  # ),
  #
  #
  # tar_target(
  #   sim_summary,
  #   generate_simulation_summary(sim_res_path)
  # ),
  #
  # tar_target(
  #   sim_pred_measure_raw,
  #   generate_simulation_res_raw(sim_summary)
  # ),
  #
  #
  # tar_target(
  #   sim_cnr_prop,
  #   sim_pred_measure_raw %>%
  #     rowwise %>%
  #     mutate(mean_cns = mean(unlist(sim_pi_cns)), sd_cns = sd(unlist(sim_pi_cns))) %>%
  #     ungroup() %>%
  #     select(-c(sim_pi_cns, train, test))
  # ),
  #
  #
  # tar_target(
  #   sim_fail_prop,
  #   generate_simulation_mdl_failed_rate(sim_pred_measure_raw)
  # ),
  #
  # tar_target(
  #   sim_train_res,
  #   sim_pred_measure_raw %>%
  #     generate_sim_pred_measure("train")
  # ),
  #
  # tar_target(
  #   sim_test_res,
  #   sim_pred_measure_raw %>%
  #     generate_sim_pred_measure("test")
  # ),
  #
  # tar_target(
  #   metrics,
  #   c("deviance", "Cindex")
  # ),
  #
  #
  # # Tabulate Simulation Results
  # tar_target(
  #   sim_train_tab,
  #   tab_sim_res(sim_train_res, sec = metrics),
  #   pattern = map(metrics),
  #   iteration = "list"
  # ),
  #
  # # Visualize Simulation Results
  # tar_target(
  #   sim_train_viz,
  #   viz_sim_res(sim_train_res, sec = metrics),
  #   pattern = map(metrics),
  #   iteration = "list"
  # ),
  #
  #
  # # Tabulate Simulation Results
  # tar_target(
  #   sim_test_tab,
  #   tab_sim_res(sim_test_res, sec = metrics),
  #   pattern = map(metrics),
  #   iteration = "list"
  # ),
  #
  # # Visualize Simulation Results
  # tar_target(
  #   sim_test_viz,
  #   viz_sim_res(sim_test_res, sec = metrics),
  #   pattern = map(metrics),
  #   iteration = "list"
  # ),

  # Real Data Analysis ------------------------------------------------------
  #* Emory Card Biobank -----------------------------------------------------
  tar_target(ECB_train_path,
             "Real_Data/Emory_Card_Biobank/Analysis_data_first_cohort.csv",
             format = "file"),

  tar_target(ECB_train_dat,
             readr::read_csv(ECB_train_path)),

  tar_target(ECB_var_screen,
             var_screen(ECB_train_dat)),

  tar_target(ECB_cov,
             ECB_train_dat %>%
               select(all_of(ECB_var_screen)) %>%
               data.matrix),

  tar_target(ECB_outcome,
             ECB_train_dat %>%
               select(time = timetodeath3yr,
                      status = death3yr)),

  # ** Data Sumary --------------------------------------------------------


  # ** LASSO ---------------------------------------------------------------
  ECB_lasso_cv = cv.glmnet(x=ECB_cov, y=Surv(ECB_outcome$time, event = ECB_outcome$status), family = "cox"),
  ECB_lasso_fnl = glmnet(x=ECB_cov, y=Surv(ECB_outcome$time, event = ECB_outcome$status), family = "cox",
                         lambda = ECB_lasso_cv$lambda.1se),
  # In sample measures
  # pred = predict(ECB_lasso_fnl, newx = ECB_cov),
  # apply(pred, 2, Cindex, y = Surv(ECB_outcome$time, event = ECB_outcome$status)),

  # ** BHAM ---------------------------------------------------------------
  ECB_sm_df = data.frame(
    Var = ECB_cov %>% colnames,
    Func = "s",
    Args ="bs='cr', k=5"
  ),

  ECB_sm_obj = construct_smooth_data(ECB_sm_df, ECB_train_dat),
  ECB_dsn_mat = ECB_sm_obj$data,

  ECB_bcam_raw = bamlasso(x = ECB_dsn_mat, y = Surv(ECB_outcome$time, event = ECB_outcome$status),
                          family = "cox", ss = c(0.04, 0.5),
                          group = make_group(names(ECB_dsn_mat))),

  ECB_bcam_cv = tune.bgam(ECB_bcam_raw, s0 = seq(0.005, 0.15, 0.01)),
  #
  ECB_bcam_fnl = bamlasso(x = ECB_dsn_mat, y = Surv(ECB_outcome$time, event = ECB_outcome$status) ,
                          family = "cox", ss = c(0.025, 0.5), # TODO: edit
                          group = make_group(names(ECB_dsn_mat)),
                          offset = 0),



  # ECB_bamlasso_insample_msr = measure.bh(ECB_bcam_fnl),
  ECB_bcam_var = bamlasso_var_selection(ECB_bcam_fnl),

  # *** Plot Non-Linear Functions ####
  ECB_bcam_nonlnr = ECB_bcam_var$`Non-parametric`[[1]],

  tar_target(
    ECB_plot_list,
    plot_smooth_term(ECB_bcam_fnl, ECB_bcam_nonlnr, ECB_sm_obj$Smooth,
                     min = min(ECB_cov[, ECB_bcam_nonlnr])-0.1,
                     max = max(ECB_cov[, ECB_bcam_nonlnr]) + 0.1)+
      xlab(str_split(ECB_bcam_nonlnr, "[.]")[[1]][1])+
      theme_pubr()+
      theme(axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()),
    pattern = map(ECB_bcam_nonlnr),
    iteration = "list"
  ),

  ECB_gg_plot = ggarrange(plotlist = ECB_plot_list) %>%
    annotate_figure(left = "Linear Predictor", bottom = "Features"),

  ECB_plot = ggsave(
    filename = "Manuscript/Figs/ECB_plot.pdf",
    plot = ECB_gg_plot,
    device = "pdf"),

  # Manuscript --------------------------------------------------------------
  #* Section Paths ####
  tar_files(manu_path,
            c("Manuscript/01-intro.Rmd", "Manuscript/02-method.Rmd",
              "Manuscript/03-simulation.Rmd", "Manuscript/04-real_data.Rmd",
              "Manuscript/05-conclusion.Rmd", "Manuscript/bibfile.bib")
  ),

  tar_render(manu, "Manuscript/00-main.Rmd",
             output_file = "CPH_AM.pdf")
)
