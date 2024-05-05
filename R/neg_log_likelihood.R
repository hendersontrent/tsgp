#' Calculate negative marginal log likelihood of a Gaussian process regression model
#'
#' @param params \code{vector} of hyperparameters
#' @param x \code{numeric} vector of input data
#' @param xprime \code{numeric} vector of data points to predict
#' @param y \code{numeric} vector of values to learn from
#' @param covfun \code{function} specifying the covariance function to use
#' @param epsilon \code{numeric} scalar denoting a small quantity to add to the (x, x) covariance matrix for numerical stability. Defaults to \code{1e-9}
#' @return \code{numeric} scalar of the negative log likelihood
#' @author Trent Henderson
#'

neg_log_likelihood <- function(params, x, xprime, y, covfun, epsilon = 1e-9) {

  # Calculate covariance matrices

  Sigma_11 <- covfun(x, x, ...) + diag(epsilon, length(x)) # Add small diagonal epsilon for numerical stability
  Sigma_12 <- covfun(x, xprime, ...)
  Sigma_inv <- t(solve(Sigma_11, Sigma_12))
  Sigma_22 <- covfun(xprime, xprime, ...)
  Sigma_2 <- Sigma_22 - (Sigma_inv %*% Sigma_12) # Posterior covariance matrix
  mu_2 <- Sigma_inv %*% y # Posterior mean vector

  # Calculate log determinant

  log_det <- sum(log(diag(chol(Sigma_11))))

  # Calculate negative marginal log likelihood

  nll <- 0.5 * (t(y) %*% solve(Sigma_11) %*% y + log_det + length(x) * log(2 * pi))
  return(nll)
}
