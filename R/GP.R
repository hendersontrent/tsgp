#' Calculate posterior mean vector and covariance matrix
#'
#' @param x \code{numeric} vector of input data
#' @param xprime \code{numeric} vector of data points to predict
#' @param y \code{numeric} vector values to learn from
#' @param Sigma \code{matrix} containing the covariance matrix
#' @return \code{TSGP} object containing the input data, posterior mean function and covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- 1:100
#' y <- 3 * sin(2 * seq(0, 4 * pi, length.out = 100)) + runif(100) * 2 + (0.08 * seq(from = 1, to = 100, by = 1))
#' Sigma <- cov_exp_quad(x1, x1, 0.05, 1)
#' mod <- GP(x1)
#'

GP <- function(x, xprime, y, Sigma){
  Sigma_11 <- cov_function(x, x, ...)
  Sigma_12 <- cov_function(x, xprime, ...)
  Sigma_inv <- t(solve(Sigma_11, Sigma_12))
  Sigma_22 <- cov_function(xprime, xprime, ...)
  Sigma_2 <- Sigma_22 - (Sigma_inv %*% Sigma_12) # Posterior covariance matrix
  mu_2 <- Sigma_inv %*% y # Posterior mean vector
  gp <- list(x, xprime, y, mu_2, Sigma_2)
  names(gp) <- c("x", "xprime", "y", "mu", "Sigma")
  gp <- structure(gp, class = c("TSGP", "list"))
  return(gp)
}
