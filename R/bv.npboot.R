#' NonParametric Bootstrap
#' @export
#' @param x data
#' @param R number of replications
#' @param T statistic:thetahat (mean, variance, ...)
#' @examples  logmean=function(x) log(mean(x))
#' @examples air=c(3,5,7,18,43,85,91,98,100,130,230,487)
#' @examples bv.npboot(air, R=1000, T=logmean)[1:2]

bv.npboot=function(x, R=1000, T){ # x:data(X1,...,Xn), T:statistic= thehahat (mean , var, sd)
  xs=matrix(sample(x, size=R*length(x) ,replace=TRUE), nrow=R) # bootstrap samples
  ts=apply(xs,1,T) # thetahat star bootsrap estimates
  list(bias=mean(ts)-T(x), var=var(ts), ts=ts)
}
