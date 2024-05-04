#include <Rcpp.h>
using namespace Rcpp;

//' Compute a linear covariance matrix
//'
//' @param xa \code{NumericVector} of values
//' @param xb \code{NumericVector} of values
//' @param sigma_b \code{double} denoting the constant variance
//' @param sigma_v \code{double} denoting the variance
//' @param c \code{double} denoting the offset
//' @return \code{NumericMatrix} of covariance
//' @author Trent Henderson
//' @examples
//' x <- 1:100
//' cov_linear_cpp(x, x, 0.05, 1, 1)
NumericMatrix cov_linear_cpp(NumericVector xa, NumericVector xb, double sigma_b, double sigma_v, double c) {
  int n1 = xa.size();
  int n2 = xb.size();
  NumericMatrix K(n1, n2);

  for (int i = 0; i < n1; ++i) {
    for (int j = 0; j < n2; ++j) {
      double inner_product = (xa[i] - c) * (xb[j] - c);
      K(i, j) = sigma_b * sigma_b + sigma_v * sigma_v * inner_product;
    }
  }

  return K;
}
