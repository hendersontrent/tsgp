test_that("exponential quadratic works", {
  covmat <- cov_exp_quad(x1, x1, 0.05, 1)
  expect_equal(100 * 100, nrow(covmat) * ncol(covmat))
})

test_that("rational quadratic works", {
  covmat <- cov_rat_quad(x1, x1, 0.05, 1, 1)
  expect_equal(100 * 100, nrow(covmat) * ncol(covmat))
})

test_that("periodic works", {
  covmat <- cov_periodic(x1, x1, 0.05, 1, 1)
  expect_equal(100 * 100, nrow(covmat) * ncol(covmat))
})

test_that("linear works", {
  covmat <- cov_linear(x1, x1, 0.05, 1, 1)
  expect_equal(100 * 100, nrow(covmat) * ncol(covmat))
})
