#' Variance Reduction Method
#' @export
#' @param alpha shape parameter beta instrumental density
#' @param beta shape parameter beta instrumental density
#' @param n number of generated points
#' @examples IS(n=10000, alpha=1, beta=1.3)*4

IS = function(n=1 , alpha=1, beta=1.3) {
  x = rbeta(n, alpha, beta)
  hg = sqrt(1 - x^2)/dbeta(x, alpha, beta)
  c(mean(hg), sd(hg)/sqrt(n))
}
