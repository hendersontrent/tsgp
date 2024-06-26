% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariance.R
\name{cov_linear}
\alias{cov_linear}
\title{Compute a linear covariance matrix}
\usage{
cov_linear(xa, xb, sigma_b = 1, sigma_v = 1, c = 1)
}
\arguments{
\item{xa}{\code{numeric} vector of values}

\item{xb}{\code{numeric} vector of values}

\item{sigma_b}{\code{numeric} scalar denoting the constant variance. Defaults to \code{1}}

\item{sigma_v}{\code{numeric} scalar denoting the variance. Defaults to \code{1}}

\item{c}{\code{numeric} scalar denoting the offset. Defaults to \code{1}}
}
\value{
\code{NumericMatrix} of covariance
}
\description{
Compute a linear covariance matrix
}
\examples{
x1 <- seq(from = -2, to = 2, length.out = 100)
cov_linear(x1, x1, 0.05, 1, 1)

}
\author{
Trent Henderson
}
