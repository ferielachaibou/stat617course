#' Distribution of Xn
#' @export
#' @param n number of generated points MC
#' @param P transition matrix
#' @param pi0 initial distribution
#' @examples n=3
#' @examples P= rbind(c(0.6, 0.4, 0), c(0.2, 0.6, 0.2), c(0, 0.4, 0.6))
#' @examples pi0=rep(1/3, 3)
#' @examples pi.n(n, P, pi0)

pi.n=function(n=3, P, pi0){
  p=pi0
  for(i in 1:n)
    p= p %*% P
  p
}
