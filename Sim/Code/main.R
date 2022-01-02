# DO NOT CHANGE THIS SECTION
## Receive the simulation parameters from job file
## Evaluate the simulation parameters in the R global environment
## For the Toy Example
## It is equivalent to run
# n_train <- 200
# p <- c(4, 10, 50, 100, 200)[1]
# rho <- c(0, 0.5)[1]
# pi_cns <- c(0.15, 0.3, 0.4)[1]

args=(commandArgs(TRUE))

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}



# Library & Helper Functions ----------------------------------------------
## Required Libraries
library(tidyverse)
library(mgcv)
library(cosso)
library(BhGLM)
library(BHAM)
library(survival)
library(simsurv)

## Helper Functions
source("~/Manuscript-BH_Additive_Cox/Sim/Code/find_censor_parameter.R")
source("~/Manuscript-BH_Additive_Cox/Sim/Code/create_HD_formula.R")
source("~/Manuscript-BH_Additive_Cox/Sim/Code/make_null_res.R")


# Data Generating Process -------------------------------------------------
# * Simulation Parameters -------------------------------------------------
source("~/Manuscript-BH_Additive_Cox/Sim/Code/sim_pars_funs.R")


## Use Array ID as random seed ID
it <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
# it <- 1
set.seed(it)

# * Generate Data -------------------------------------------------
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


## Censoring Distribution, Weibull(alpha.c, scale.p)
scale.p <- find_censor_parameter(lambda = exp(-1*train_dat$eta/shape.t),
                                 pi.cen = pi_cns,
                                 shape_hazard = shape.t, shape_censor = shape.c)

train_dat <-  train_dat %>%
  data.frame(
    c_time = rweibull(n = n_train, shape = shape.c, scale = scale.p)
  ) %>%
  mutate(
    cen_ind = (c_time < eventtime),
    status = (!cen_ind)*1
  ) %>%
  rowwise() %>%
  mutate(time = min(c_time, eventtime)) %>%
  ungroup()



# Fit Models------------------------------------------------------------------


# * Spline Specification --------------------------------------------------
mgcv_df <- data.frame(
  Var = grep("X", names(train_dat), value = TRUE),
  Func = "s",
  Args = paste0("bs='cr', k=", k)
)


# * mgcv --------------------------------------------------------------------
mgcv_mdl <- tryCatch({
  gam(create_HD_formula(time~1, spl_df = mgcv_df), data = train_dat,
      family = cox.ph(), weight = status)
},
error = function(err) {
  mgcv_mdl <- NULL
  return(NULL)
})

mgcv_train <- make_null_res("cox")
mgcv_test <- make_null_res("cox")

if(!is.null(mgcv_mdl)){
  mgcv_train <- measure.cox(Surv(train_dat$time, train_dat$status) , mgcv_mdl$linear.predictors)
  mgcv_test <- measure.cox(Surv(test_dat$eventtime, test_dat$status),
                           predict(mgcv_mdl, newdata=test_dat, type = "link"))
}

# * COSSO -------------------------------------------------------------------
cosso_mdl <-  tryCatch({cosso(x = train_dat %>% select(starts_with("X")) %>% data.matrix,
                              y = train_dat %>% select(time, status) %>% data.matrix, family = "Cox",
                              nbasis = k, scale = F)
},
error = function(err) {
  cosso_mdl <- NULL
  return(NULL)
}
)

if(!is.null(cosso_mdl)){
  cosso_tn_mdl <- tryCatch({
    tune.cosso(cosso_mdl, plot.it = FALSE)
  },
  error = function(err) {
    return(NULL)
  }
  )
}


if(!is.null(cosso_mdl) && !is.null(cosso_tn_mdl)){

  cosso_train_lp <- predict.cosso(cosso_mdl,
                                  xnew=train_dat %>% select(starts_with("X")) %>% data.matrix,
                                  M=ifelse(!is.null(cosso_tn_mdl), cosso_tn_mdl$OptM, 2), type = "fit")
  cosso_train <- measure.cox(Surv(train_dat$time, train_dat$status), cosso_train_lp)

  cosso_test_lp <- predict.cosso(cosso_mdl,
                                 xnew=test_dat %>% select(starts_with("X")) %>% data.matrix,
                                 M=ifelse(!is.null(cosso_tn_mdl), cosso_tn_mdl$OptM, 2), type = "fit")
  cosso_test <- measure.cox(Surv(test_dat$eventtime, test_dat$status), cosso_test_lp)

} else{
  cosso_train <- make_null_res("cox")
  cosso_test <- make_null_res("cox")
}


#### Fit ACOSSO Models ####
acosso_mdl <-  tryCatch({
  wt_mdl <- SSANOVAwt(x = train_dat %>% select(starts_with("X")) %>% data.matrix,
                      y = train_dat %>% select(time, status) %>% data.matrix, family = "Cox", nbasis=k)
  cosso(x = train_dat %>% select(starts_with("X")) %>% data.matrix,
        y = train_dat %>% select(time, status) %>% data.matrix, family = "Cox",
        wt= wt_mdl, scale = F, nbasis=k)
},
error = function(err) {
  acosso_mdl <- NULL
  return(NULL)
})

if(!is.null(acosso_mdl)){
  acosso_tn_mdl <- tryCatch({
    tune.cosso(acosso_mdl, plot.it = FALSE)
  },
  error = function(err) {
    return(NULL)
  }
  )
}



if(!is.null(acosso_mdl) && !is.null(acosso_tn_mdl)){

  acosso_train_lp <- predict.cosso(acosso_mdl,
                                   xnew = train_dat %>% select(starts_with("X")) %>% data.matrix,
                                   M = ifelse(!is.null(acosso_tn_mdl), acosso_tn_mdl$OptM, 2), type = "fit")
  acosso_train <- measure.cox(Surv(train_dat$time, train_dat$status), acosso_train_lp)

  acosso_test_lp <- predict.cosso(acosso_mdl,
                                  xnew=test_dat %>% select(starts_with("X")) %>% data.matrix,
                                  M=ifelse(!is.null(acosso_tn_mdl), acosso_tn_mdl$OptM, 2), type = "fit")
  acosso_test <- measure.cox(Surv(test_dat$eventtime, test_dat$status), acosso_test_lp)
} else {
  acosso_train <- acosso_test <- make_null_res("cox")
}

# * BHAM ----------------------------------------------------------

train_sm_dat <- construct_smooth_data(mgcv_df, train_dat)
train_smooth_data <- train_sm_dat$data

test_sm_dat <- BHAM::make_predict_dat(train_sm_dat$Smooth, dat = test_dat)


#** bmlasso -----------------------------------------------------------------
bamlasso_raw_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                 family = "cox", group = make_group(names(train_smooth_data)),
            ss = c(0.04, 0.5))

blasso_s0_seq <- seq(0.005, 0.1, length.out = 20)    # TODO: need to be optimized
blasso_cv_res <- tune.bgam(bamlasso_raw_mdl, nfolds = 5, s0= blasso_s0_seq, verbose = FALSE)

blasso_s0_min <- blasso_cv_res$s0[which.min(blasso_cv_res$deviance)]
bamlasso_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                          family = "cox", group = make_group(names(train_smooth_data)),
                          ss = c(blasso_s0_min, 0.5))

bamlasso_train <- measure.cox(Surv(train_dat$time, train_dat$status) , bamlasso_mdl$linear.predictors)
bamlasso_test <- measure.bh(bamlasso_mdl, test_sm_dat, Surv(test_dat$eventtime, test_dat$status))


#** Bacox ----------------------------------------------------------
# bacox_raw_mdl <- bacoxph(Surv(time, event = status) ~ ., data = data.frame(time = train_dat$time, status = train_dat$status,
#                                                                            train_smooth_data),
#                          prior = mde(), group = make_group(names(train_smooth_data)))
#
# s0_seq <- seq(0.005, 0.1, length.out = 20)    # TODO: need to be optimized
# cv_res <- tune.bgam(bacox_raw_mdl, nfolds = 5, s0= s0_seq, verbose = FALSE)
#
# s0_min <- cv_res$s0[which.min(cv_res$deviance)]
#
# bacox_mdl <- bacoxph(Surv(train_dat$time, train_dat$status) ~ ., data = train_smooth_data,
#                      prior = mde(s0 = s0_min), group = make_group(names(train_smooth_data)))



# bacox_train <- measure.cox(Surv(train_dat$time, train_dat$status) , bacox_mdl$linear.predictors)
# bacox_test <- measure.cox(Surv(test_dat$eventtime, test_dat$status),
#                          predict(mgcv_mdl, newdata=test_dat, type = "link"))
# bacox_test <- measure.bh(bacox_mdl, test_sm_dat, Surv(test_dat$eventtime, test_dat$status))

# Save Simulation Results -------------------------------------------------

## TODO: Record Prediction Results
# Overall
ret <- list(
  train_res = list(
    mgcv = mgcv_train,
    cosso = cosso_train,
    acosso = acosso_train,
    # bacox = bacox_train,
    bamlasso = bamlasso_train
  ),
  test_res = list(
    mgcv = mgcv_test,
    cosso = cosso_test,
    acosso = acosso_test,
    # bacox = bacox_test,
    bamlasso = bamlasso_test
  ),
  p.cen = mean(train_dat$status==0)              # Censoring proportion in training data
)


job_name <- Sys.getenv('SLURM_JOB_NAME')
# Recommendation: to save the results in individual rds files
saveRDS(ret,
        paste0("/data/user/boyiguo1/bcam/Res/", job_name,"/it_",it,".rds"))
