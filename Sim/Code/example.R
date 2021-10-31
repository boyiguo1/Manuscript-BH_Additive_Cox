library(simsurv)
library(MASS)
library(tidyverse)


# Simulation Parameters ---------------------------------------------------


n <- 100
p <- c(4, 10, 50, 100, 200)[1]
rho <- c(0, 0.5)[1]

AR <- function(p, rho){
  rho^abs(outer(1:p,1:p,"-"))
}

k <- 10

cov_mat <- diag(p)

set.seed(2)

f_1 <- function(x) (x+1)^2/2
f_2 <- function(x) exp(x+1)/2
f_3 <- function(x) 3*sin(x)/2
f_4 <- function(x) 1.4*x+0.5

# Aimed censoring rate 20%

# Simulate Raw X ----------------------------------------------------------
x <- mvrnorm(n, rep(0, p), AR(p, rho)) %>%
  data.frame

# Calculate Proportional Hazards ------------------------------------------
eta <- with(x, f_1(X1) + f_2(X2) + f_3(X3) + f_4(X4))


# Simulate Survival Time --------------------------------------------------
dat <- simsurv::simsurv(dist = "exponential",
                        lambdas = 0.1,
                        x = data.frame(eta) ,
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




