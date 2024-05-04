test_that("GP works", {

  CovSum <- function(xa, xb, sigma_1 = 1, sigma_2 = 1, l_1 = 1, l_2 = 1, p = 1){
    Sigma_exp_quad <- cov_exp_quad(xa, xb, sigma_1, l_1)
    Sigma_periodic <- cov_periodic(xa, xb, sigma_2, l_2, p)
    Sigma <- Sigma_exp_quad + Sigma_periodic
    return(Sigma)
  }

  mod <- GP(x1, 1:length(y), y, CovSum, 0.5)
  expect_equal(5, length(mod))
})
