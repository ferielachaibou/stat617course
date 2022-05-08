#' Variance Reduction Method
#' @export
#' @param a lower limit
#' @param b upper limit
#' @param n number of generated points
#' @examples cv(n=1000, a=0, b=1)*4

cv=function(n=10, a=0, b=1){
  x=runif(n,a,b)
  h=sqrt(1-x^2)
  thetah=mean(h)
  C=mean(x)
  muC=(b-a)/2
  beta=4-6*thetah
  thetahC= thetah-beta*(C-muC)
  thetahC
}
