## How fast is it?
n.sample <- 1e5
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
qp.points <- ggplot(df, aes(observed = P)) +
  stat_qqunif() +
  geom_abline(intercept = 0, slope = 1)

qp.raster <- ggplot(df, aes(observed = P)) +
  stat_qqunif(geom = ggrastr:::GeomPointRast) +
  geom_abline(intercept = 0, slope = 1)

qp.hex <- ggplot(df, aes(y = P)) +
  stat_qqunif_hex() +
  geom_abline(intercept = 0, slope = 1)


system.time(print(qp.points))
system.time(print(qp.raster))
system.time(print(qp.hex))
system.time(qqman::qq(df$P))

