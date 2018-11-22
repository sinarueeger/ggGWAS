## How fast is it?
n.sample <- 1e7
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
qp.points <- ggplot(df, aes(observed = P)) +
  stat_qqplot() +
  geom_abline(intercept = 0, slope = 1)

qp.raster <- ggplot(df, aes(observed = P)) +
  stat_qqplot(geom = ggrastr:::GeomPointRast) +
  geom_abline(intercept = 0, slope = 1)

system.time(print(qp.points))
system.time(print(qp.raster))
system.time(qqman::qq(df$P))

## density plot
n.sample <- 1e5
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

df <- giant
N <- nrow(df)
expected <- sort(-log10((1:N) / N - 1 / (2 * N)))
observed <- sort(-log10(df$P))
df2 <- data.frame(expected = expected, observed = observed)

ggplot(df2, aes(expected, observed)) + geom_point()+ geom_abline(intercept = 0, slope = 1)

ggplot(df2, aes(expected, observed)) + geom_hex(bins = 100) + geom_abline(intercept = 0, slope = 1) + scale_fill_gradient(low="black", high="black")+ geom_point(color = I("blue"))
# Scatter plot
ggplot(df2) + geom_hex(aes(expected, observed), bins = 100) + geom_abline(intercept = 0, slope = 1) + scale_fill_gradient(low="black", high="black")+ geom_point(color = I("blue"))


ggplot(df2, aes(expected, observed)) + stat_density_2d(geom = "point", aes(size = stat(density)), n = 100, contour = FALSE)


#########################################################################
