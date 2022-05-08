#' Monte Carlo Integration
#' @export
#' @param h function
#' @param a lower limit
#' @param b upper limit
#' @param n number of generated points
#' @examples h=function(x) sqrt(1-x^2)
#' @examples sm(h,a=0,b=1,n=1e5)*4

sm=function(h,a=0,b=1,n=1){
  x=runif(n,a,b)
  ih=(b-a)*mean(h(x)); ise=(b-a)*sd(h(x))/sqrt(n)
  c(ih,ise)
}
