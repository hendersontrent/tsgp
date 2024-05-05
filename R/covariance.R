#' Compute an exponential quadratic covariance matrix, also known as a squared exponential
#'
#' @param xa \code{numeric} vector of values
#' @param xb \code{numeric} vector of values
#' @param sigma \code{numeric} scalar denoting the variance. Defaults to \code{1}
#' @param l \code{numeric} scalar denoting the lengthscale. Defaults to \code{1}
#' @return \code{GPCov} containing the covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- seq(from = -2, to = 2, length.out = 100)
#' cov_exp_quad(x1, x1, 0.05, 1)
#'

cov_exp_quad <- function(xa, xb, sigma = 1, l = 1){
  X <- cov_exp_quad_cpp(xa, xb, sigma, l)
  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}

#' Compute a rational quadratic covariance matrix
#'
#' @param xa \code{numeric} vector of values
#' @param xb \code{numeric} vector of values
#' @param sigma \code{numeric} scalar denoting the variance. Defaults to \code{1}
#' @param alpha \code{numeric} scalar greater than \code{0} denoting the mixing coefficient. Defaults to \code{1}
#' @param l \code{numeric} scalar denoting the lengthscale. Defaults to \code{1}
#' @return \code{GPCov} containing the covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- seq(from = -2, to = 2, length.out = 100)
#' cov_rat_quad(x1, x1, 0.05, 1, 1)
#'

cov_rat_quad <- function(xa, xb, sigma = 1, alpha = 1, l = 1){
  X <- cov_rat_quad_cpp(xa, xb, sigma, alpha, l)
  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}

#' Compute a periodic covariance matrix
#'
#' @param xa \code{numeric} vector of values
#' @param xb \code{numeric} vector of values
#' @param sigma \code{numeric} scalar denoting the variance. Defaults to \code{1}
#' @param l \code{numeric} scalar denoting the lengthscale. Defaults to \code{1}
#' @param p \code{integer} scalar denoting the period. Defaults to \code{1}
#' @return \code{GPCov} containing the covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- seq(from = -2, to = 2, length.out = 100)
#' cov_periodic(x1, x1, 0.05, 1, 1)
#'

cov_periodic <- function(xa, xb, sigma = 1, l = 1, p = 1){
  X <- cov_periodic_cpp(xa, xb, sigma, l, p)
  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}

#' Compute a white noise covariance matrix
#'
#' @param xa \code{numeric} vector of values
#' @param xb \code{numeric} vector of values
#' @param sigma \code{numeric} scalar denoting the variance. Defaults to \code{0.5}
#' @return \code{GPCov} containing the covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- seq(from = -2, to = 2, length.out = 100)
#' cov_noise(x1, x1, 0.5)
#'

cov_noise <- function(xa, xb, sigma = 0.5){
  X <- cov_noise_cpp(xa, xb, sigma)
  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}

#' Compute a linear covariance matrix
#'
#' @param xa \code{numeric} vector of values
#' @param xb \code{numeric} vector of values
#' @param sigma_b \code{numeric} scalar denoting the constant variance. Defaults to \code{1}
#' @param sigma_v \code{numeric} scalar denoting the variance. Defaults to \code{1}
#' @param c \code{numeric} scalar denoting the offset. Defaults to \code{1}
#' @return \code{NumericMatrix} of covariance
#' @author Trent Henderson
#' @examples
#' x1 <- seq(from = -2, to = 2, length.out = 100)
#' cov_linear(x1, x1, 0.05, 1, 1)
#'

cov_linear <- function(xa, xb, sigma_b = 1, sigma_v = 1, c = 1){
  X <- cov_linear_cpp(sigma, n)
  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}
