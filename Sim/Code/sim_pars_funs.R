
# Fixed Parameters --------------------------------------------------------a
# * Basic Parameters --------------------------------------------------------
k <- 10               # Smoothing function degrees
n_test <- 1000        # Training set Sample size


# * Survival Parameters ---------------------------------------------------
# shape.t <- 1.2     # Shape par of hazard distribution
# scale.t <- 1       # Scale par of hazard distribution
# shape.c <- 0.8     # Shape par of censoring distribution


# * Derived Parameters
n_total <- n_train + n_test


# Functions ---------------------------------------------------

# * Nonliner Functions ----------------------------------------------------
# f_1 <- function(x) (x+1)^2/10
# f_2 <- function(x) exp(x+1)/100
# f_3 <- function(x) 3*sin(x)/20
# f_4 <- function(x) (1.4*x+0.5)/10


#' Auto-regressive correlation matrix
#'
#' @param p integer, the number of dimension
#' @param rho double, range between 0-1, 0 means mutually independent, 1 means complete correlated
#'
#' @return A p*p dimension matrix
#'
#' @examples
#' p <- 5
#' rho <- 0.8
#' X_mat <- MASS::mvrnorm(n=100, rep(0, p), AR(p, rho))
# AR <- function(p, rho){
#   rho^abs(outer(1:p,1:p,"-"))
# }

