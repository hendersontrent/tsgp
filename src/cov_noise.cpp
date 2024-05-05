#include <Rcpp.h>
using namespace Rcpp;

//' Compute a white noise covariance matrix
//'
//' @param xa \code{NumericVector} of values
//' @param xb \code{NumericVector} of values
//' @param sigma \code{double} denoting the variance
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' x1 <- seq(from = -2, to = 2, length.out = 100)
//' cov_noise_cpp(x1, x1, 0.5)
// [[Rcpp::export]]
NumericMatrix cov_noise_cpp(NumericVector xa, NumericVector xb, double sigma) {
  int n1 = xa.size();
  int n2 = xb.size();
  NumericMatrix K(n1, n2);

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      if (i == j) {
        K(i, j) = pow(sigma, 2);
      } else {
        K(i, j) = 0.0;
      }
    }
  }

  return K;
}
