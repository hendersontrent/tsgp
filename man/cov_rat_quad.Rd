% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance.R
\name{cov_rat_quad}
\alias{cov_rat_quad}
\title{Compute a rational quadratic covariance matrix}
\usage{
cov_rat_quad(xa, xb, sigma = 1, alpha = 1, l = 1)
}
\arguments{
\item{xa}{\code{numeric} vector of values}

\item{xb}{\code{numeric} vector of values}

\item{sigma}{\code{numeric} scalar denoting the variance. Defaults to \code{1}}

\item{alpha}{\code{numeric} scalar greater than \code{0} denoting the mixing coefficient. Defaults to \code{1}}

\item{l}{\code{numeric} scalar denoting the lengthscale. Defaults to \code{1}}
}
\value{
\code{GPCov} containing the covariance matrix
}
\description{
Compute a rational quadratic covariance matrix
}
\examples{
x1 <- seq(from = -2, to = 2, length.out = 100)
cov_rat_quad(x1, x1, 0.05, 1, 1)

}
\author{
Trent Henderson
}
