n_train <- 200
p <- c(4, 10, 50, 100, 200)[2]
rho <- c(0, 0.5)[1]
pi_cns <- c(0.15, 0.3, 0.4)[1]

library(tidyverse)
library(mgcv)
library(cosso)
library(BhGLM)
library(BHAM)
library(survival)
library(simsurv)
library(glmnet)


source("Sim/Code/find_censor_parameter.R")
source("Sim/Code/create_HD_formula.R")
source("Sim/Code/make_null_res.R")
source("Sim/Code/sim_pars_funs.R")

k<-20

job_name <- paste0("bcam_sim_p=",p,",rho=",rho, ",pi_cns=", pi_cns)
it <- 1
set.seed(it)

x_all <- MASS::mvrnorm(n_train+n_test, rep(0, p), AR(p, rho)) %>%
  data.frame
eta_all <- with(x_all, f_1(X1) + f_2(X2) + f_3(X3) + f_4(X4))
dat_all <- simsurv::simsurv(dist = "weibull",
                            lambdas = scale.t,
                            gammas = shape.t,
                            x = data.frame(eta = eta_all) ,
                            beta = c(eta = 1)) %>%
  data.frame( x_all, eta = eta_all, .)

train_dat <- dat_all[1:n_train, ]
test_dat <- dat_all[(n_train+1):n_total, ]


## Censoring Distribution, Weibull(shape.c, scale.c)
scale.c <- tryCatch({
  find_censor_parameter(lambda = exp(-1*train_dat$eta/shape.t),
                        pi.cen = pi_cns,
                        shape_hazard = shape.t, shape_censor = shape.c)
},
error = function(err) {
  if(!file.exists("~/Manuscript-BH_Additive_Cox/Sim/Code/scale_vec.RDS"))
    stop("Please Generate scale_vec, and use 'R/calculate_scales' to generates scale_vec.RDS")
  # scale_vec <- readRDS("~/Manuscript-BH_Additive_Cox/Sim/Code/scale_vec.RDS")
  scale_vec <- readRDS("Sim/Code/scale_vec.RDS")
  scale.c <- scale_vec[[job_name]]
  if(is.null(scale.c)) stop("No scale for this scenario")
  return(scale.c)
})



# Save Scale Parameter ----------------------------------------------------
# TODO: if you need to generate scale vector, please uncomment the following two line
# saveRDS(scale.c,
#         paste0("/data/user/boyiguo1/bcam/scale/", job_name,"/it_",it,".rds"))


train_dat <-  train_dat %>%
  data.frame(
    c_time = rweibull(n = n_train, shape = shape.c, scale = scale.c)
  ) %>%
  mutate(
    cen_ind = (c_time < eventtime),
    status = (!cen_ind)*1
  ) %>%
  rowwise() %>%
  mutate(time = min(c_time, eventtime)) %>%
  ungroup()

mgcv_df <- data.frame(
  Var = grep("X", names(train_dat), value = TRUE),
  Func = "s",
  Args = paste0("bs='cr', k=", k)
)


train_sm_dat <- construct_smooth_data(mgcv_df, train_dat)
train_smooth_data <- train_sm_dat$data

test_sm_dat <- BHAM::make_predict_dat(train_sm_dat$Smooth, dat = test_dat)

bam_group <- make_group(names(train_smooth_data))


#** bmlasso -----------------------------------------------------------------
bamlasso_raw_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                              family = "cox", group = make_group(names(train_smooth_data)),
                              ss = c(0.04, 0.5))

blasso_s0_seq <- seq(0.005, 0.1, length.out = 20)    # TODO: need to be optimized
blasso_cv_res <- tune.bgam(bamlasso_raw_mdl, nfolds = 5, s0= blasso_s0_seq, verbose = FALSE)

ggplot(blasso_cv_res) +
  geom_line(aes(x = s0, y = deviance))

blasso_s0_min <- blasso_cv_res$s0[which.min(blasso_cv_res$deviance)]
bamlasso_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                          family = "cox", group = make_group(names(train_smooth_data)),
                          ss = c(blasso_s0_min, 0.5))

# Prediction
bamlasso_train <- measure.cox(Surv(train_dat$time, train_dat$status) , bamlasso_mdl$linear.predictors)
bamlasso_test <- measure.bh(bamlasso_mdl, test_sm_dat, Surv(test_dat$eventtime, test_dat$status))


# Variable Selection
bamlasso_vs_part <- bamlasso_var_selection(bamlasso_mdl)
bamlasso_var <- rep(FALSE, p) %>% `names<-`(names(test_dat %>% select(starts_with("X"))))
bamlasso_var[bamlasso_vs_part$`Non-parametric`$Variable] <- TRUE

tar_var <- "X8"
plot_smooth_term(bamlasso_mdl, tar_var, train_sm_dat$Smooth,
                 min = min(train_dat[, tar_var])-0.1,
                 max = max(train_dat[, tar_var]) + 0.1)

