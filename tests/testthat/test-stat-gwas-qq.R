test_that("Check that colors work", {
#  ggplot(data = data_whr %>% filter(CHR %in% c(1, 5, 6))) +
#    stat_gwas_qq(aes(y = P, color = CHR, group = CHR)) +
#    geom_abline(intercept = 0, slope = 1, linetype = 2)

#  ggplot(data = data_whr %>% filter(CHR %in% c(1, 5, 6))) +
#    stat_gwas_qq(aes(y = P, color = CHR)) +
#    geom_abline(intercept = 0, slope = 1, linetype = 2)#


#  ggplot(data = data_whr %>% filter(CHR %in% c(1, 5, 6))) +
#    stat_gwas_qq(aes(y = P), color = "gray") +
#    geom_abline(intercept = 0, slope = 1, linetype = 2)

  ## also for geom_gwas_qq

  ## vdiffr::expect_doppelganger()

  # Visual tests ------------------------------------------------------------

  ## needs an svg figure in testthat/figs/stat_gwas_qq/ribbon-turned-on-in-geom-smooth.svg
 # test_that("geom_smooth() works with alternative stats", {
#    df <- data_frame(x = c(1, 1, 2, 2, 1, 1, 2, 2),
#                     y = c(1, 2, 2, 3, 2, 3, 1, 2),
#                     fill = c(rep("A", 4), rep("B", 4)))
#
#    expect_doppelganger("ribbon turned on in geom_smooth", {
#      ggplot(df, aes(x, y, color = fill, fill = fill)) +
##        geom_smooth(stat = "summary") # ribbon on by default
#    })
#  })
})

test_that("Message when large P-values", {

  n <- 1000
  dat <- data.frame(P = runif(n, min = -1, max = 2))

  qp <- ggplot(data = dat) +
    stat_gwas_qq(aes(y = P))

  w <- capture_warnings(print(qp))

  expect_match(w, "Computation failed in", all = FALSE)
  expect_match(w, "P-value vector contains entries outside the range of", all = FALSE)

})

