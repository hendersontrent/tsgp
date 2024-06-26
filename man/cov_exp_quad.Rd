% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance.R
\name{cov_exp_quad}
\alias{cov_exp_quad}
\title{Compute an exponential quadratic covariance matrix, also known as a squared exponential}
\usage{
cov_exp_quad(xa, xb, sigma = 1, l = 1)
}
\arguments{
\item{xa}{\code{numeric} vector of values}

\item{xb}{\code{numeric} vector of values}

\item{sigma}{\code{numeric} scalar denoting the variance. Defaults to \code{1}}

\item{l}{\code{numeric} scalar denoting the lengthscale. Defaults to \code{1}}
}
\value{
\code{GPCov} containing the covariance matrix
}
\description{
Compute an exponential quadratic covariance matrix, also known as a squared exponential
}
\examples{
x1 <- seq(from = -2, to = 2, length.out = 100)
cov_exp_quad(x1, x1, 0.05, 1)

}
\author{
Trent Henderson
}
