library(ggplot2)
df <- data.frame(y = runif(1000))

## simple
ggplot(df, aes(sample = y)) +
  stat_qq(distribution = stats::qunif) +
  coord_trans(x = "log10", y = "log10")


## tranform y
ggplot(df, aes(sample = log10(y))) +
  stat_qq(distribution = stats::qunif) +
  coord_trans(x = "log10", y = "log10")

## add coord_trans
ggplot(df, aes(sample = y)) +
  stat_qq(distribution = stats::qunif) +
  coord_trans(x = "log10", y = "log10")

## add scale
ggplot(df, aes(sample = y)) +
  stat_qq(distribution = stats::qunif) +
  scale_x_log10() +
  scale_y_log10()

## add scale
f.trans <- function(x) -log10(x)
f.trans.inverse <- function(x) 10^(-x)

gg <- ggplot(df, aes(sample = y)) +
  stat_qq(distribution = stats::qunif)

mlog10_trans <- scales::trans_new("-log10",
                                  transform = function(x) -log10(x),
                                  inverse = function(x) 10^(-x))

gg + scale_y_continuous(trans = mlog10_trans) + scale_x_continuous(trans = mlog10_trans) + geom_abline(intercept = 0, slope = 1)

qqman::qq(df$y)
## source: https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html






