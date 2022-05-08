#' Bootstrap Estimates
#' @export
#' @param d data
#' @param R number of replications
#' @param T statistic:thetahat (mean, variance, ...)
#' @examples air=c(3,5,7,18,43,85,91,98,100,130,230,487)
#' @examples logmean=function(x) log(mean(x))
#' @examples ts=sort(bt.est(d=air, R=1000, T=logmean))

bt.est= function(d, R=1000, T){
  x=sample(d, R*length(d), replace=TRUE)
  dim(x)=c(R, length(d))
  apply(x,1,T)
}
