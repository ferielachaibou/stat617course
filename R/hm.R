#' Monte Carlo Integration
#' @export
#' @param h function
#' @param a lower limit
#' @param b upper limit
#' @param c max value of h function
#' @param n number of generated points
#' @examples h=function(x) sqrt(1-x^2)
#' @examples hm(h,a=0,b=1,c=1,n=1e5)*4

hm=function(h,a=0,b=1,c=1,n=1) {
  x=runif(n,a,b)
  y=runif(n,0,c)
  p=sum(h(x)>y)/n
  ih=c*(b-a)*p; ise=c*(b-a)*sqrt(p*(1-p)/n)
  c(ih, ise)
}
