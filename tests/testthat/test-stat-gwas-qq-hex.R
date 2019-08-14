## https://github.com/r-lib/vdiffr

test_that("Message when large P-values", {

  n <- 1000
  dat <- data.frame(P = runif(n, min = -1, max = 2))

  qp <- ggplot(data = dat) +
    stat_gwas_qq_hex(aes(y = P))

  w <- capture_warnings(print(qp))

  expect_match(w, "Computation failed in", all = FALSE)
  expect_match(w, "P-value vector contains entries outside the range of", all = FALSE)

})



test_that("No message when large P-values", {


})
