library(simsurv)
library(MASS)
library(tidyverse)
library(ggpubr)

source("Sim/Code/helper_functions.R")

# Simulation Parameters ---------------------------------------------------

n_test <- 100
n_train <- 10000
p <- c(4, 10, 50, 100, 200)[2]
rho <- c(0, 0.5)[1]
k <- 10
pi_cns <- c(0.15, 0.3, 0.4)
dist_hzd <- c("exponential",
              "weibull",
              "gompertz")


f_1 <- function(x) (x+1)^2/2
f_2 <- function(x) exp(x+1)/15
f_3 <- function(x) 3*sin(x)/2
f_4 <- function(x) 1.4*x+0.5

# Plot the functions
list(f_1, f_2, f_3, f_4) %>%
  map(.f = function(fun){
    ggplot() +
      xlim(-4, 4) +
      geom_function(fun = fun) +
       theme(axis.title.y = element_blank())
  }) %>%
  ggarrange(plotlist = .,label.y = "Log Hazard Ratio")


set.seed(3)



# Data Generating Process -------------------------------------------------
n_total <- n_train + n_test

AR <- function(p, rho){
  rho^abs(outer(1:p,1:p,"-"))
}










# Aimed censoring rate 20%

# Simulate Raw X ----------------------------------------------------------
x_all <- mvrnorm(n_train+n_test, rep(0, p), AR(p, rho)) %>%
  data.frame

# Calculate Log Proportional Hazards ------------------------------------------
eta_all <- with(x_all, f_1(X1) + f_2(X2) + f_3(X3) + f_4(X4))


find_cenor_parameter(lambda = exp(-1*eta_all/1.2))

# Simulate Survival Time --------------------------------------------------
dat_all <- simsurv::simsurv(dist = "weibull",
                        lambdas = 1.2,
                        gammas = 0.5,
                        x = data.frame(eta = eta_all) ,
                        beta = c(eta = 1)) %>%
  # mutate(status = as.numeric(eventtime <= 1.5)) %>%
  data.frame(
    c_time = rexp(n, 1),
             x) %>%
  # rowwise() %>%
  mutate(
    cen_ind = (c_time < eventtime),
    status = (!cen_ind)*1
         ) %>%
  rowwise() %>%
  mutate(time = min(c_time, eventtime)) %>%
    ungroup()

# Censoring Rate
mean(dat$status==0)




# mgcv --------------------------------------------------------------------
library(mgcv)

mgcv_mdl <- gam(time~s(X1) + s(X2) + s(X3) + s(X4), data = dat,
                family = cox.ph(), weight = status)

# COSSO -------------------------------------------------------------------
library(cosso)
# y <- dat %>% select(time, status)
cosso_mdl <- cosso(x = dat %>% select(starts_with("X")),
                   y = dat %>% select(time, status), family = "Cox")


# Proposed Model ----------------------------------------------------------
library(BhGLM)
library(BHAM)
library(survival)

spline_df <- data.frame(
  Var = grep("(X)", names(dat), value = TRUE),
  Func = "s",
  Args = paste0("bs='cr', k=", k)
)

train_sm_dat <- construct_smooth_data(spline_df, dat)
train_smooth_data <- train_sm_dat$data

bacox_mdl <- bacoxph(Surv(dat$time, dat$status) ~ ., data = train_smooth_data,
        prior = mde(), group = make_group(names(train_smooth_data)))




