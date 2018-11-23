
## some copied from here:
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r


dat <- qqman::gwasResults
# n.sample <- 10000
# df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

## check if stop/error/warnings work
qp <- ggplot(dat, aes(observed = P)) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)
print(qp) + theme_gwas()

## raster
qp <- ggplot(dat, aes(observed = P)) +
  stat_qq_rast() +
  geom_abline(intercept = 0, slope = 1)
print(qp) + theme_gwas()

## show only p-values above a cerain threshold
qp <- ggplot(dat, aes(observed = P)) +
  stat_qq(threshold = 0.05) +
  geom_abline(intercept = 0, slope = 1)
print(qp)

## observed.thresh working?
qp <- ggplot(dat, aes(observed = P)) +
  stat_qq(observed.thresh = 0.05) +
  geom_abline(intercept = 0, slope = 1)
print(qp)

## adding nice stuff
qp +
  theme(aspect.ratio = 1) + ## square shaped
  expand_limits(x = -log10(max(dat$P)), y = -log10(max(dat$P))) + ## identical limits (meaning truely square)
  ggtitle("QQplot") + ## title
  xlab("Expected -log10(P)") + ## axis labels
  ylab("Observed -log10(P)")

## color
ggplot(dat, aes(observed = P, color = as.character(CHR))) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)

## facet
ggplot(dat, aes(observed = P)) +
  facet_wrap(~CHR) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)

## group
ggplot(dat, aes(observed = P, group = CHR, color = as.character(CHR))) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)


## raster
gg_vec <- ggplot(dat, aes(x = P, y = P))
gg_ras <- gg_vec + ggrastr::geom_point_rast(size = 0.5)

PrintFileSize <- function(gg, name) {
  invisible(ggsave("tmp.pdf", gg, width = 4, height = 4))
  cat(name, ": ", file.info("tmp.pdf")$size / 1024, " Kb.\n", sep = "")
  unlink("tmp.pdf")
}

PrintFileSize(gg_ras, "Raster")
PrintFileSize(gg_vec, "Vector")

## check if stop/error/warnings work
qp <- ggplot(dat, aes(observed = P)) +
  stat_qq(method = "raster") +
  geom_abline(intercept = 0, slope = 1)
print(qp)


## beat this:
## http://www.gettinggeneticsdone.com/2010/07/qq-plots-of-p-values-in-r-using-base.html
