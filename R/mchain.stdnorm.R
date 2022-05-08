#' Chain for Standard Normal Distribution
#' @export
#' @param x0 initial states
#' @param n number of generated points MC
#' @examples mc=mchain.stdnorm(x0=-5, n=200)

mchain.stdnorm=function(x0=-5, n=200){
  chain=double(n+1)
  chain[1]= x0
  for(i in 2:(n+1)){
    x=chain[i-1]
    z=runif(1, x-1, x+1)
    p=min(1, dnorm(z)/dnorm(x))
    chain[i]= ifelse(runif(1) <= p, z, x)
  }
  chain
}
