#include <Rcpp.h>
using namespace Rcpp;

//' Compute a white noise covariance matrix
//'
//' @param sigma \code{double} denoting the variance
//' @param n \code{int} denoting the number of rows and columns required in the covariance matrix
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' cov_noise_cpp(0.05, 100)
// [[Rcpp::export]]
NumericMatrix cov_noise_cpp(double sigma, int n) {
  NumericMatrix K(n, n);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (i == j) {
        K(i, j) = pow(sigma, 2);
      } else {
        K(i, j) = 0.0;
      }
    }
  }

  return K;
}
