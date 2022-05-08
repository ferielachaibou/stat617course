#' Monte Carlo Integration
#' @export
#' @param h function
#' @param f function
#' @param rf function
#' @param n number of generated points
#' @examples h=function(x) sqrt(1-x^2)
#' @examples f=function(x) 2*x
#' @examples rf=function(n) sqrt(runif(n))
#' @examples gsm(h, f, rf, n = 1e5)*4

gsm=function(h,f,rf,n=1){
  x=rf(n)
  y=h(x)/f(x)
  c(mean(y),sd(y)/sqrt(n))
}
