#' Metropolis Hasting: Bivariate Normal
#' @export
#' @param x0 initial states
#' @param rho correlation coefficient
#' @param n number of generated points MC
#' @param FUN dbnorm
#' @examples dbnorm = function(x, d) exp(-t(x) %*% d %*% x/2)
#' @examples bnorm.mh(x0=c(-5,5), rho = 0.8, n = 500, FUN = dbnorm)

bnorm.mh = function(x0=c(-5,5), rho=0.8, n=500, FUN) {
  d = solve(cbind(c(1, rho), c(rho, 1)))
  chain = matrix(nrow = n + 1, ncol = 2)
  chain[1, ] = x = x0
  for (i in 2:(n + 1)) {
    z = x + runif(2, -1, 1)
    p = min(1, dbnorm(z,d)/dbnorm(x,d))
    if (runif(1) <= p)
      chain[i, ] = x = z
    else chain[i, ] = x
  }
  chain
}
