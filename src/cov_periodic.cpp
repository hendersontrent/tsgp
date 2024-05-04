#include <Rcpp.h>
using namespace Rcpp;

//' Compute a periodic covariance matrix
//'
//' @param xa \code{NumericVector} of values
//' @param xb \code{NumericVector} of values
//' @param sigma \code{double} denoting the variance
//' @param l \code{double} denoting the lengthscale
//' @param p \code{int} denoting the period
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' x1 <- seq(from = -2, to = 2, length.out = 100)
//' cov_periodic_cpp(x1, x1, 0.05, 1, 1)
// [[Rcpp::export]]
NumericMatrix cov_periodic_cpp(NumericVector xa, NumericVector xb, double sigma, double l, int p) {
  int n1 = xa.size();
  int n2 = xb.size();
  NumericMatrix K(n1, n2);

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      double diff = std::abs(xa[i] - xb[j]);
      double sin_term = std::sin(M_PI * diff / p);
      double exp_term = std::exp(-2 / pow(l, 2) * std::pow(sin_term, 2));
      K(i, j) = pow(sigma, 2) * exp_term;
    }
  }

  return K;
}
