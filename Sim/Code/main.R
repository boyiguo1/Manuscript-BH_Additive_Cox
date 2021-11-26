# DO NOT CHANGE THIS SECTION
## Receive the simulation parameters from job file
## Evaluate the simulation parameters in the R global environment
## For the Toy Example
## It is equivalent to run
# n_train <- 100
# p <- c(4, 10, 50, 100, 200)[2]
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

## Helper Functions
source("~/Manuscript-BH_Additive_Cox/Sim/Code/find_censor_parameter.R")



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

# TODO:: Double check if the lambda and gammas are specified correctly
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

# TODO: Make particion of the training and testing data


# Fit Models------------------------------------------------------------------

# * mgcv --------------------------------------------------------------------
## TODO: Make the data matrix
# mgcv_mdl <- gam(time~s(X1) + s(X2) + s(X3) + s(X4), data = dat,
#                 family = cox.ph(), weight = status)

# * COSSO -------------------------------------------------------------------

# y <- dat %>% select(time, status)
# cosso_mdl <- cosso(x = dat %>% select(starts_with("X")),
#                    y = dat %>% select(time, status), family = "Cox")


# * BHAM ----------------------------------------------------------


# spline_df <- data.frame(
#   Var = grep("(X)", names(dat), value = TRUE),
#   Func = "s",
#   Args = paste0("bs='cr', k=", k)
# )
#
# train_sm_dat <- construct_smooth_data(spline_df, dat)
# train_smooth_data <- train_sm_dat$data
#
# bacox_mdl <- bacoxph(Surv(dat$time, dat$status) ~ ., data = train_smooth_data,
#                      prior = mde(), group = make_group(names(train_smooth_data)))



# Save Simulation Results -------------------------------------------------
## TODO: Record censoring rate

## TODO: Record Prediction Results
# Overall
ret <- list(
  p.cen = mean(train_dat$status==0)              # Censoring proportion in training data
)


job_name <- Sys.getenv('SLURM_JOB_NAME')
# Recommendation: to save the results in individual rds files
saveRDS(ret,
        paste0("/data/user/boyiguo1/bcam/Res/", job_name,"/it_",it,".rds"))
