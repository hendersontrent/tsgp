% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance.R
\name{cov_periodic}
\alias{cov_periodic}
\title{Compute a periodic covariance matrix}
\usage{
cov_periodic(xa, xb, sigma = 1, l = 1, p = 1)
}
\arguments{
\item{xa}{\code{numeric} vector of values}

\item{xb}{\code{numeric} vector of values}

\item{sigma}{\code{numeric} scalar denoting the variance. Defaults to \code{1}}

\item{l}{\code{numeric} scalar denoting the lengthscale. Defaults to \code{1}}

\item{p}{\code{integer} scalar denoting the period. Defaults to \code{1}}
}
\value{
\code{GPCov} containing the covariance matrix
}
\description{
Compute a periodic covariance matrix
}
\examples{
x1 <- seq(from = -2, to = 2, length.out = 100)
cov_periodic(x1, x1, 0.05, 1, 1)

}
\author{
Trent Henderson
}
