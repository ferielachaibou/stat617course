#' Gibbs Sampler: Bivariate Normal
#' @export
#' @param x0 initial states
#' @param rho correlation coefficient
#' @param n number of generated points MC
#' @examples bnorm.gibbs(x0 = c(-5, 5), rho = 0.8, n = 500)

bnorm.gibbs = function(x0=c(-5,5), rho=0.8, n=500) {
  s = sqrt(1 - rho^2)
  chain = matrix(nrow = n + 1, ncol = 2)
  chain[1, ] = x0
  for (i in 2:(n + 1)) {
    chain[i, 1] = rnorm(1, rho * chain[i -
                                         1, 2], s)
    chain[i, 2] = rnorm(1, rho * chain[i,
                                       1], s)
  }
  chain
}
