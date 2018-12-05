context("mlog10_trans")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#' df <- data.frame(y = runif(1000))
#'
#' ggplot(df, aes(sample = y)) +
#' stat_qq(distribution = stats::qunif) +
#' scale_y_continuous(trans = mlog10_trans()) +
#' scale_x_continuous(trans = mlog10_trans()) +
#' geom_abline(intercept = 0, slope = 1)

## test that equal to qqman::qq(df$y)
