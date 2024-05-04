#include <Rcpp.h>
using namespace Rcpp;

//' Compute a rational quadratic covariance matrix
//'
//' @param xa \code{NumericVector} of values
//' @param xb \code{NumericVector} of values
//' @param sigma \code{double} denoting the variance
//' @param alpha \code{double} greater than \code{0} denoting the mixing coefficient
//' @param l \code{double} denoting the lengthscale
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' x1 <- seq(from = -2, to = 2, length.out = 100)
//' cov_rat_quad_cpp(x1, x1, 0.05, 1, 1)
// [[Rcpp::export]]
NumericMatrix cov_rat_quad_cpp(NumericVector xa, NumericVector xb, double sigma, double alpha, double l) {
  int n1 = xa.size();
  int n2 = xb.size();
  NumericMatrix K(n1, n2);

  // Check if alpha is greater than 0

  if (alpha <= 0) {
    Rcpp::stop("alpha must be greater than 0");
  }

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      double diff = xa[i] - xb[j];
      double norm_sq = pow(diff, 2);
      double exp_term = pow(1 + norm_sq / (2 * alpha * pow(l, 2)), -alpha);
      K(i, j) = pow(sigma, 2) * exp_term;
    }
  }

  return K;
}
