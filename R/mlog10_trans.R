#' Minus log breaks (integer breaks on -log-trnsformed scales)
#'
#' @inheritParams scales::log_breaks
#'
#' @return function
#' @references From \url{https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html} and \url{https://gist.github.com/wch/3250485}
#' @export
#' @examples
#' require(scales)
#' mlog_breaks()(runif(1000))
#'


mlog_breaks <- function (n = 5, base = 10)
  {
    scales:::force_all(n, base)  ## no clue what this does

    function(x) {
      rng <- -log(range(x, na.rm = TRUE), base = base)
      max <- ceiling(rng[1])
      min <- floor(rng[2])
      if (max == min)
        return(base^(-min))
      by <- floor((max - min)/n) + 1
      breaks <- base^(-seq(min, max, by = by))
      relevant_breaks <- base^(-rng[1]) <= breaks & breaks <=
        base^(-rng[2])
      if (sum(relevant_breaks) >= (n - 2))
        return(breaks)
      while (by > 1) {
        by <- by - 1
        breaks <- base^(-seq(min, max, by = by))
        relevant_breaks <- base^(-rng[1]) <= breaks & breaks <=
          base^(-rng[2])
        if (sum(relevant_breaks) >= (n - 2))
          return(breaks)
      }
      scales:::log_sub_breaks(rng, n = n, base = base)
    }
}

#' Minus log transformation
#'
#' @export
#' @references \url{https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html and https://gist.github.com/wch/3250485}
#'
#' @examples
#' require(ggplot2)
#' df <- data.frame(y = runif(1000))
#'
#' ggplot(df, aes(sample = y)) +
#'   stat_qq(distribution = stats::qunif) +
#'   scale_y_continuous(trans = mlog_trans()) +
#'   scale_x_continuous(trans = mlog_trans()) +
#'   geom_abline(intercept = 0, slope = 1)
mlog_trans <- function(base = 10) {

  force(base) ## no clue what this does
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)


  scales::trans_new(paste0("-log-", format(base)),
    transform = trans,
    inverse = inv,
    breaks = mlog_breaks(base = base),
    domain = c(1e-300, Inf) ## cause P-values small in GWAS
  )
}

#' Minus log-10 transformation
#'
#' @export
#'
#' @examples
#' require(ggplot2)
#' df <- data.frame(y = runif(1000))
#'
#' ggplot(df, aes(sample = y)) +
#'   stat_qq(distribution = stats::qunif) +
#'   scale_y_continuous(trans = mlog10_trans()) +
#'   scale_x_continuous(trans = mlog10_trans()) +
#'   geom_abline(intercept = 0, slope = 1)
mlog10_trans <- function()
{
  mlog_trans(10)
}
