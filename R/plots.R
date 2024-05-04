#' Plot a covariance matrix
#'
#' @importFrom stats reshape
#' @importFrom MASS mvrnorm
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw theme geom_tile scale_fill_viridis_c
#' @param x \code{GPCov} object containing the covariance matrix
#' @param xa \code{numeric} vector used to generate \code{x}
#' @param type \code{character} denoting whether to plot samples from the GP prior (\code{"prior"}) assuming a mean vector of zero, or to draw a heatmap of the covariance matrix (\code{"matrix"}). Defaults to \code{"prior"}
#' @param k \code{integer} denoting number of realisations to draw if \code{type = "prior"}. Defaults to \code{5}
#' @param ... arguments to be passed to methods
#' @return \code{ggplot2} object containing the plot
#' @author Trent Henderson
#' @export
#' @examples
#' x <- 1:100
#' covmat <- cov_exp_quad(x, x, 0.05, 1)
#' plot(covmat, xa, type = "prior", k = 5)
#'

plot.GPCov <- function(x, xa, type = c("prior", "matrix"), k = 5){
  stopifnot(inherits(x, "GPCov") == TRUE)
  type <- match.arg(type)

  #---------------- GP prior -----------------

  # Prepare mean vector of zeroes

  mu <- integer(length = nrow(x))

  # Produce dynamic column names, draw from multivariate normal, and convert to dataframe

  thenames <- paste0("Sample ", 1:k)
  y <- as.data.frame(t(MASS::mvrnorm(n = k, mu = mu, Sigma = x)))
  colnames(y) <- thenames
  draws <- data.frame(x = xa)
  draws <- cbind(draws, y)

  # Wrangle into long format for ggplot

  draws <- stats::reshape(draws,
                          direction = "long",
                          v.names = "y",
                          varying = 2:ncol(draws),
                          times = names(draws)[2:ncol(draws)],
                          timevar = "sample_ind")

  # Draw plot

  p <- ggplot2::ggplot(data = draws, ggplot2::aes(x = x, y = y, colour = sample_ind)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::labs(x = "x",
                  y = "y = f(x)",
                  colour = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  #---------------- Covariance matrix -----------------

  mat_df <- as.data.frame(x)
  mat_df$x <- seq_len(nrow(mat_df))

  mat_df <- stats::reshape(mat_df,
                           direction = "long",
                           v.names = "values",
                           varying = 1:(ncol(mat_df) - 1),
                           times = 1:nrow(mat_df),
                           idvar = "x")

  p <- ggplot2::ggplot(data = mat_df) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = time, fill = values)) +
    ggplot2::labs(title = "Heatmap of covariance matrix",
         x = "x",
         y = "x",
         fill = "k(x,x')") +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1),
                                  breaks = seq(from = 0, to = 1, by = 0.2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}
