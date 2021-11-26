#' Estimate the Weibull scale parameter for controlled censoring proportion
#'
#' @param lambda
#' @param pi A double whose value is between 0 and 1 exclusive, i.e. (0,1). The pre-defined censoring proportion.
#' @param shape_hazard A positive double, Weibull distribution shape parameter `k` for the hazard distribution. Default to 1 for exponential distribution
#' @param shape_censor A positive double, Weibull distribution shape parameter `k` for the censoring distribution. Default to 1 for exponential distribution
#' @param ... Reserved arguments
#'
#' @return theta, A positive double, Weibull distribution scale parameter for the censoring distribution
#'
#' @references  Wan, F. (2017). Simulating survival data with predefined censoring rates for proportional hazards models. Statistics in medicine, 36(5), 838-854.
#'
#' @examples
find_censor_parameter <- function(
  lambda, pi = 0.5,
  shape_hazard = 1, shape_censor= 1,
  ...){
  # Estimate the density function of the linear predictor
  dens <- density(lambda, bw = "nrd0",na.rm=TRUE)

  x<-dens$x
  y<-dens$y
  y.loess <-loess(y ~ x,span=0.1)


  density.fun.lambda<-function(x){
    pred.y <- predict(y.loess, newdata=x)
    return(pred.y)
  }

  integration_over_time <- function(
    u,               # Different values of lambda
    # Arguments for integration
    theta, shape_hazard, shape_censor
  ){
    ret <- integrate(            # Start (Integrate of time)
      f = function(ti, theta, alpha.t, alpha.c){
        lambda.i<-u
        part1<-density.fun.lambda(lambda.i)
        part2<-dweibull(ti,alpha.c,theta)
        part3<-exp(-(ti/lambda.i)^alpha.t)
        return(part1*part2*part3)
      },
      0, Inf,
      theta = theta, alpha.t = shape_hazard, alpha.c = shape_censor
    )
    return(ret$value)
  }


  censor.prop <- function(theta, p, shape_hazard, shape_censor){
    cen.P <- integrate(         # Start (Integrate of lambda)
      function(u, theta, shape_hazard, shape_censor){
        sapply(u, integration_over_time,
               # Arguments of integration_over_time
               theta = theta, shape_hazard = shape_hazard, shape_censor = shape_censor
        )
      },
      # Limits of integration
      lower = min(lambda), upper = max(lambda),
      ## Arguments for integration function
      theta = theta, shape_hazard = shape_hazard, shape_censor = shape_censor
    )$value     # End(int of lambda)
    return(cen.P-p)
  }

  theta<-uniroot(censor.prop, interval = c(0.1,200),
                 ## Argument for censor.prop
                 p = pi, shape_hazard = shape_hazard, shape_censor = shape_censor,
                 tol=0.00000001)$root

  return(theta)
}
