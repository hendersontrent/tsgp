#' Calculate posterior mean vector and covariance matrix
#'
#' @param x \code{numeric} vector of input data
#' @param xprime \code{numeric} vector of data points to predict
#' @param y \code{numeric} vector values to learn from
#' @param covfun \code{function} specifying the covariance function to use
#' @return \code{TSGP} object containing the input data, posterior mean function and covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- 1:100
#' y <- 3 * sin(2 * seq(0, 4 * pi, length.out = 100)) + runif(100) * 2 + (0.08 * seq(from = 1, to = 100, by = 1))
#'
#' CovSum <- function(xa, xb, sigma_1 = 1, sigma_2 = 1, l_1 = 1, l_2 = 1, p = 1){
#'   Sigma_exp_quad <- cov_exp_quad(xa, xb, sigma_1, l_1)
#'   Sigma_periodic <- cov_periodic(xa, xb, sigma_2, l_2, p)
#'   Sigma <- Sigma_exp_quad + Sigma_periodic
#'   return(Sigma)
#' }
#'
#' mod <- GP(x1, 1:length(y), y, CovSum, 0.5)
#'

GP <- function(x, xprime, y, covfun, sigma_noise = 0.5, ...){
  Sigma_11 <- covfun(x, x, ...) + diag(sigma_noise ^ 2, length(x))
  Sigma_12 <- covfun(x, xprime, ...)
  Sigma_inv <- t(solve(Sigma_11, Sigma_12))
  Sigma_22 <- covfun(xprime, xprime, ...)
  Sigma_2 <- Sigma_22 - (Sigma_inv %*% Sigma_12) # Posterior covariance matrix
  mu_2 <- Sigma_inv %*% y # Posterior mean vector
  gp <- list(x, xprime, y, mu_2, Sigma_2)
  names(gp) <- c("x", "xprime", "y", "mu", "Sigma")
  gp <- structure(gp, class = c("GaussProc", "list"))
  return(gp)
}
