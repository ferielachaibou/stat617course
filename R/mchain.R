#' Simulating a Markov Chain
#' @export
#' @param n number of generated points MC
#' @param x0 initial states
#' @param P transition matrix
#' @examples P1=rbind(c(3,1)/4, c(3,2)/5)
#' @examples mchain(10, 2, P1)

mchain=function(n=10, x0=2, P){
  chain=rep(0, n+1)
  chain[1]=x0
  for(i in 1:n)
    chain[i+1]=sample(ncol(P), 1, prob=P[chain[i],])
  chain
}
