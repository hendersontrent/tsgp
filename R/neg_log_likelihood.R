#' Calculate negative marginal log likelihood of a Gaussian process regression model
#'
#' @param params \code{vector} of hyperparameters
#' @param x \code{numeric} vector of input data
#' @param xprime \code{numeric} vector of data points to predict
#' @param y \code{numeric} vector of values to learn from
#' @param covfun \code{function} specifying the covariance function to use
#' @param noise \code{numeric} scalar denoting the noise variance to add to the (x, x) covariance matrix of observations. Defaults to \code{0} for no noise modelling
#' @param ... arguments to be passed to methods
#' @return \code{numeric} scalar of the negative log likelihood
#' @author Trent Henderson
#'

neg_log_likelihood <- function(params, x, xprime, y, covfun, noise = 0, ...) {

  # Calculate covariance matrices

  Sigma_11 <- covfun(x, x, ...) + ((noise ^ 2) * diag(length(x))) # Add noise to observation covariance matrix
  Sigma_12 <- covfun(x, xprime, ...)
  Sigma_inv <- t(chol2inv(chol(Sigma_11)) %*% Sigma_12) # Cholesky for stability
  Sigma_22 <- covfun(xprime, xprime, ...)
  Sigma_2 <- Sigma_22 - (Sigma_inv %*% Sigma_12) # Posterior covariance matrix
  mu_2 <- Sigma_inv %*% y # Posterior mean vector

  # Calculate log determinant

  log_det <- sum(log(diag(chol(Sigma_11))))

  # Calculate negative marginal log likelihood

  nll <- 0.5 * (t(y) %*% solve(Sigma_11) %*% y + log_det + length(x) * log(2 * pi))
  return(nll)
}
