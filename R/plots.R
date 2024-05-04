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
#' x1 <- 1:100
#' covmat <- cov_exp_quad(x1, x1, 0.05, 1)
#' plot(covmat, x1, type = "prior", k = 5)
#'

plot.GPCov <- function(x, xa, type = c("prior", "matrix"), k = 5, ...){
  stopifnot(inherits(x, "GPCov") == TRUE)
  type <- match.arg(type)

  if(type == "prior"){

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

  } else{

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
      ggplot2::labs(x = "x",
                    y = "x",
                    fill = "k(x,x')") +
      ggplot2::scale_fill_viridis_c(limits = c(0, 1),
                                    breaks = seq(from = 0, to = 1, by = 0.2)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  }

  return(p)
}

#-------------------- Posterior ------------------

#' Plot summary of draws from GP posterior
#'
#' @importFrom stats qnorm median
#' @param x \code{TSGP} object containing the model
#' @param
#' @param draws \code{integer} denoting the number of draws to take from the posterior. Defaults to \code{100}
#' @param robust \code{Boolean} denoting whether to use the mean (\code{FALSE}) or median (\code{FALSE}) to summarise the distribution. Defaults to \code{FALSE} for the mean
#' @param ... arguments to be passed to methods
#' @return \code{ggplot} object containing the plot
#'

plot.TSGP <- function(x, conf = 0.95, draws = 100, robust = FALSE, ...){

  stopifnot(inherits(x, "TSGP") == TRUE)
  stopifnot(conf > 0 & conf < 1)

  # Calculate z-score for Gaussian interval

  half <- (1 - conf) / 2
  z <- stats::qnorm(half, lower.tail = FALSE)

  # Generate dataframe of draws from posterior

  thenames <- paste0("Sample ", 1:k)
  posterior <- as.data.frame(t(MASS::mvrnorm(n = draws, mu = x$mu, Sigma = x$Sigma)))
  colnames(posterior) <- thenames

  posterior <- posterior %>%
    mutate(timepoint = x$xprime) %>%
    pivot_longer(cols = 1:ncol(posterior), names_to = "sample", values_to = "y")

  if(robust){
    posterior_ct <- posterior %>%
      reframe(mu = stats::median(y), .by = "timepoint")
  } else{
    posterior_ct <- posterior %>%
      reframe(mu = mean(y), .by = "timepoint")
  }

  posterior_sd <- data.frame(timepoint = x$xprime,
                             sigma = sqrt(diag(x$Sigma))) # SD is square root of variance

  posterior_summary <- merge(x = posterior_ct, y = posterior_sd, by = "timepoint")
  posterior_summary$lower <- posterior_summary$mu - (z * posterior_summary$sigma)
  posterior_summary$upper <- posterior_summary$mu + (z * posterior_summary$sigma)

  # Draw plot

  p <- ggplot2::ggplot(data = posterior_summary) +
    ggplot2::geom_ribbon(ggplot2::aes(x = timepoint, ymin = lower, ymax = upper), fill = "steelblue2", alpha = 0.5) +
    ggplot2::geom_line(data = data, ggplot2::aes(x = timepoint, y = y), colour = "black") +
    ggplot2::geom_point(data = data, ggplot2::aes(x = timepoint, y = y), colour = "black") +
    ggplot2::geom_line(ggplot2::aes(x = timepoint, y = mu), colour = "steelblue2", size = 1) +
    ggplot2::labs(title = "Posterior ", ifelse(robust, "median", "mean"), " and ", round(conf * 100, digits = 0), "% prediction interval",
                  subtitle = paste0("Interval calculated over ", draws, " samples"),
                  x = "Timepoint",
                  y = "Value") +
    ggplot2::theme_bw()

  return(p)
}
