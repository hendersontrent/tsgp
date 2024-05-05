#' Calculate posterior mean vector and covariance matrix
#'
#' @importFrom stats optim
#' @param x \code{numeric} vector of input data
#' @param xprime \code{numeric} vector of data points to predict
#' @param y \code{numeric} vector of values to learn from
#' @param covfun \code{function} specifying the covariance function to use
#' @param noise \code{numeric} scalar denoting the noise variance to add to the (x, x) covariance matrix of observations. Defaults to \code{0} for no noise modelling
#' @param optim \code{Boolean} whether to optimise hyperparameters using negative marginal log likelihood. Defaults to \code{FALSE}
#' @param method \code{character} denoting the optimisation algorithm in \code{stats::optim} to use if \code{optim = TRUE}. Defaults to \code{"L-BFGS-B"}
#' @param ... covariance function hyperparameters to be passed to the covariance functions within \code{cov_function}
#' @return \code{TSGP} object containing the input data, posterior mean function and covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- 1:100
#'
#' y <- 3 * sin(2 * seq(0, 4 * pi, length.out = 100)) +
#'   runif(100) * 2 + (0.08 * seq(from = 1, to = 100, by = 1))
#'
#' CovSum <- function(xa, xb, sigma_1 = 1, sigma_2 = 1, l_1 = 1, l_2 = 1, p = 1){
#'   Sigma_exp_quad <- cov_exp_quad(xa, xb, sigma_1, l_1)
#'   Sigma_periodic <- cov_periodic(xa, xb, sigma_2, l_2, p)
#'   X <- Sigma_exp_quad + Sigma_periodic
#'   X <- structure(X, class = c("GPCov", "matrix"))
#'   return(X)
#' }
#'
#' mod <- GP(x1, 1:length(y), y, CovSum, 0.8,
#'           sigma_1 = 5, sigma_2 = 1,
#'           l_1 = 75, l_2 = 1, p = 25)
#'

GP <- function(x, xprime, y, covfun, noise = 0, optim = FALSE,
               method = c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN", "Brent"), ...){

  if(optim){

    # Run optimisation procedure
    #
    # method <- match.arg(method)
    #
    # opt_result <- stats::optim(par = initial_params, fn = neg_log_likelihood, x = x,
    #                            xprime = xprime, y = y, covfun = covfun, noise = noise,
    #                            method = method)
    #
    # optimized_params <- opt_result$par
    #
    # Compute final results using optimised parameters
    #
    #

    stop("optimisation procedure is not currently implemented")

  } else{

    Sigma_11 <- covfun(x, x, ...) + ((noise ^ 2) * diag(length(x))) # Add noise to observation covariance matrix
    Sigma_12 <- covfun(x, xprime, ...)
    Sigma_inv <- t(solve(Sigma_11, Sigma_12))
    Sigma_22 <- covfun(xprime, xprime, ...)
    Sigma_2 <- Sigma_22 - (Sigma_inv %*% Sigma_12) # Posterior covariance matrix
    mu_2 <- Sigma_inv %*% y # Posterior mean vector
    gp <- list(x, xprime, y, mu_2, Sigma_2)
    names(gp) <- c("x", "xprime", "y", "mu", "Sigma")
  }

  gp <- structure(gp, class = c("TSGP", "list"))
  return(gp)
}
