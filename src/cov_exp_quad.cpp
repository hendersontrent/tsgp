#include <Rcpp.h>
using namespace Rcpp;

//' Compute an exponential quadratic covariance matrix, also known as a squared exponential
//'
//' @param xa \code{NumericVector} of values
//' @param xb \code{NumericVector} of values
//' @param sigma \code{double} denoting the variance
//' @param l \code{double} denoting the lengthscale
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' x <- 1:100
//' cov_exp_quad_cpp(x, x, 0.05, 1)
NumericMatrix cov_exp_quad_cpp(NumericVector xa, NumericVector xb, double sigma, double l) {
  int n1 = xa.size();
  int n2 = xb.size();
  NumericMatrix K(n1, n2);

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      double diff = xa[i] - xb[j];
      double exp_term = exp(-0.5 * pow(diff / l, 2));
      K(i, j) = pow(sigma, 2) * exp_term;
    }
  }

  return K;
}
