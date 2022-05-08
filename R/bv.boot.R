#' Parametric Bootstrap
#' @export
#' @param x data
#' @param R number of replications
#' @param T statistic:thetahat (mean, variance, ...)
#' @examples
#' @examples logmean=function(x) log(mean(x))
#' @examples boeing=c(3,5,7,18,43,85,91,98,100,130,230,487)
#' @examples bv.boot(boeing, R=1000, T=logmean)[1:2]

 bv.boot=function(x, R=1000, T){
  t0=T(x)
  beta0=mean(x)
  xs=matrix(rexp(length(x)*R, 1/beta0), nrow=R)
  ts=apply(xs,1,T)
  list(bias=mean(ts)-t0, var=var(ts), ts=ts)
}
