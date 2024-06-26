% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{cov_periodic_cpp}
\alias{cov_periodic_cpp}
\title{Compute a periodic covariance matrix}
\usage{
cov_periodic_cpp(xa, xb, sigma, l, p)
}
\arguments{
\item{xa}{\code{NumericVector} of values}

\item{xb}{\code{NumericVector} of values}

\item{sigma}{\code{double} denoting the variance}

\item{l}{\code{double} denoting the lengthscale}

\item{p}{\code{int} denoting the period}
}
\value{
\code{NumericMatrix} of covariance
}
\description{
Compute a periodic covariance matrix
}
\examples{
x1 <- seq(from = -2, to = 2, length.out = 100)
cov_periodic_cpp(x1, x1, 0.05, 1, 1)
}
\author{
Trent Henderson
}
