## How fast is it?
n.sample <- 1e6
set.seed(3)
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
qp.points <- ggplot(df, aes(observed = P)) +
  stat_qq_unif() +
  geom_abline(intercept = 0, slope = 1)
system.time(print(qp.points))

## raster
## -----------
rm(list = ls())
n.sample <- 1e6
set.seed(3)
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
qp.raster <- ggplot(df, aes(observed = P)) +
  stat_qq_unif(geom = ggrastr:::GeomPointRast) +
  geom_abline(intercept = 0, slope = 1)
system.time(print(qp.raster))

## hex
## -----------
rm(list = ls())
n.sample <- 1e6
set.seed(3)
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
qp.hex <- ggplot(df, aes(y = P)) +
  stat_qq_unif_hex() +
  geom_abline(intercept = 0, slope = 1)
system.time(print(qp.hex))

## qqman
## -----------
rm(list = ls())
n.sample <- 1e6
set.seed(3)
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
system.time(qqman::qq(df$P))

