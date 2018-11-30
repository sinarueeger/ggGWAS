context("test-geom_qqunif_hex")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


##---------------------------------------------------------------
##  Traditional density plot                                   --
##---------------------------------------------------------------


n.sample <- 1e5
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

df <- giant
N <- nrow(df)
expected <- sort(-log10((1:N) / N - 1 / (2 * N)))
observed <- sort(-log10(df$P))
df2 <- data.frame(expected = expected, observed = observed)

## Points
ggplot(df2, aes(expected, observed)) + geom_point()+ geom_abline(intercept = 0, slope = 1)

## Hex
ggplot(df2, aes(expected, observed)) + geom_hex(bins = 100) + geom_abline(intercept = 0, slope = 1) + scale_fill_gradient(low="black", high="black")+ geom_point(color = I("blue"), alpha = I(0.5))
# Scatter plot


##---------------------------------------------------------------
##  Testing time difference                                    --
##---------------------------------------------------------------

require(ggplot2)
n.sample <- 1e6
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

## default
(qp.slow <- ggplot(df, aes(observed = P)) +
    stat_qqunif() +
    geom_abline(intercept = 0, slope = 1))

system.time(print(qp.slow))

## hex
(qp.fast <- ggplot(df, aes(y = P, group = GWAS, color = GWAS)) +
    stat_qqunif_hex() +
    geom_abline(intercept = 0, slope = 1))

system.time(print(qp.fast))

