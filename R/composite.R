#' Construct a composite kernel of multiple individual covariance functions
#'
#' @param kernels \code{list} containing the individual covariance functions and their hyperparameters
#' @param combine \code{character} denoting whether to add or multiply the individual kernels together. Can be one of \code{"add"} or \code{"mul"}. Defaults to \code{"add"}
#' @return \code{GPCov} containing the covariance matrix
#' @author Trent Henderson
#' @export
#' @examples
#' x1 <- 1:100
#' y <- 3 * sin(2 * seq(0, 4 * pi, length.out = 100)) + runif(100) * 2 + (0.08 * seq(from = 1, to = 100, by = 1))
#'
#' cov_sum <- composite(
#'   list(
#'     Sigma_exp_quad <- cov_exp_quad(xa, xb, sigma_1, l_1)
#'     Sigma_periodic <- cov_periodic(xa, xb, sigma_2, l_2, p)
#'     Sigma_noise <- cov_noise(0.8, nrow(Sigma_exp_quad))
#'   ),
#'   "add")
#'

composite <- funtion(kernels, combine = c("add", "mul")){
  stopifnot(class(kernels) == "list")
  combine <- match.arg(combine)

  if(combine == "add"){
    X <- Reduce(`+`, kernels)
  } else{
    X <- Reduce(`*`, kernels)
  }

  X <- structure(X, class = c("GPCov", "matrix"))
  return(X)
}
