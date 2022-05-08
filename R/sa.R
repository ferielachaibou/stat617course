#' Simulated Annealing Algorithm
#' @export
#' @param n number of generated points MC
#' @param x grid of measurements
#' @details h=function(x){ if(is.vector(x)) dim(x)=c(1,2) cos(10*x[,1]^2)-5*x[,1]^2+cos(10*x[,2]^2)-5*x[,2]*2}
#' @details uwalk = function(x, step = c(0.1, 0.1), valid) { xn = x + runif(length(x), -1, 1) * step while (!valid(xn)) xn = x + runif(length(x),-1, 1) * step xn}
#' @details hv = function(x) !any(x <= -2 | x >= 2)
#' @details tempf = function(n) 0.995^(1:n) * 10000

sa = function(n=5000, x=c(-1.9,1.9)){
  tmp = tempf(n)
  chain = matrix(nrow = n + 1, ncol = length(x))
  chain[1, ] = x
  Fx = h(x)
  for (i in 2:(n + 1)) {
    z = uwalk(chain[i - 1, ],valid = hv)
    Fz = h(z)
    if (runif(1) < min(exp((Fz - Fx)/tmp[i -1]), 1)) {
      chain[i, ] = z
      Fx = Fz
    }
    else chain[i, ] = chain[i - 1, ]
  }
  chain
}
