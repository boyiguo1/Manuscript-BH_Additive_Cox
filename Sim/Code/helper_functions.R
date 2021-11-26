
# Calculate Censoring probability

# Wan, F. (2017). Simulating survival data with predefined censoring rates for proportional hazards models. Statistics in medicine, 36(5), 838-854.

find_cenor_parameter <- function(lambda, pi = 0.5, alpha.t = 1.2, alpha.c= 0.8, ...){
  browser()
  # Estimate the density function of the linear predictor
  dens<-density(lambda,bw = "nrd0",na.rm=TRUE)

  # lines(dens,col=2)
  x<-dens$x
  y<-dens$y
  y.loess <-loess(y ~ x,span=0.1)


  density.fun.lambda<-function(x){
    pred.y <- predict(y.loess, newdata=x)
    return(pred.y)
  }


  censor.prop<-function(theta,args){
    p<-args[1]
    args0<-c(theta,args[-1])
    cen.P<-integrate(function(u){
      sapply(u,function(u){
        # browser()
        integrate(function(ti,args){

          # cat("\n")
          theta  <-args[1]
          alpha.t<-args[2]
          alpha.c<-args[3]
          lambda.i<-u
          # browser()
          part1<-density.fun.lambda(lambda.i)
          part2<-dweibull(ti,alpha.c,theta)
          # cat(-(ti/lambda.i))
          # cat("\n")
          part3<-exp(-(ti/lambda.i)^alpha.t)
          # cat(part3)
          # cat("\n")
          return(part1*part2*part3)
        }, 0, Inf ,args=args0)$value
      })
    }, min(lambda), max(lambda) # This should be range of the possible lambda.i
    )$value
    return(cen.P-p)
  }

  alpha.t<- 1.2
  alpha.c<-0.8

  args<-c(pi, alpha.t,alpha.c)

  theta<-uniroot(censor.prop,args=args,c(0.1,200),tol=0.00000001)$root

  return(theta)
}
