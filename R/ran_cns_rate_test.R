run_cns_rate_test <- function(pi=0.2){
  n <- 100
  a1<- 0.1
  a2<- -0.1
  a3<- 0.2
  a4<- -0.2
  a0<- -1.44
  u1 <-rnorm(n,0,1)
  u2 <-runif(n,0,1)
  u3 <-rbinom(n,1,0.5)
  u4 <-rpois(n,5)
  alpha.t<- 1.2
  alpha.c<-0.8
  lambda<- exp(-(a0+a1*u1+a2*u2+a3*u3+a4*u4)/alpha.t)
  max.lambda<-max(lambda)

  scale.p <- find_cenor_parameter(lambda, pi = pi)


  event_dat <- simsurv(dist = "weibull",
                       lambdas = 1,
                       gammas = 1.2,
                       x = data.frame(u0 = 1, u1, u2, u3, u4),
                       betas = c(u0 = a0, u1 = a1, u2 = a2, u3=a3, u4 = a4))

  cen_dat <- rweibull(n = n, shape = alpha.c, scale = scale.p)

  sum(cen_dat < event_dat$eventtime)/n
}
