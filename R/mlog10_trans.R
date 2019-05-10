#' log sub breaks (copy)
#' exact copy from scales:::log_sub_breaks
#'
#' @param rng Range of values
#' @param n number of breaks
#' @param base log base
#'
#' @export
log_sub_breaks_gggwas <- function (rng, n = 5, base = 10)
{
  min <- floor(rng[1])
  max <- ceiling(rng[2])
  if (base <= 2) {
    return(base^(min:max))
  }
  steps <- 1
  delta <- function(x) {
    min(diff(log(sort(c(x, steps, base)), base = base)))
  }
  candidate <- seq_len(base)
  candidate <- candidate[1 < candidate & candidate < base]
  while (length(candidate)) {
    best <- which.max(vapply(candidate, delta, 0))
    steps <- c(steps, candidate[best])
    candidate <- candidate[-best]
    breaks <- as.vector(outer(base^seq(min, max), steps))
    relevant_breaks <- base^rng[1] <= breaks & breaks <=
      base^rng[2]
    if (sum(relevant_breaks) >= (n - 2)) {
      break
    }
  }
  breaks <- sort(breaks)
  lower_end <- pmax(min(which(base^rng[1] <= breaks)) - 1,
                    1)
  upper_end <- pmin(max(which(breaks <= base^rng[2])) + 1,
                    length(breaks))
  breaks[lower_end:upper_end]
}


#' Minus log breaks (integer breaks on -log-trnsformed scales)
#'
#' @inheritParams scales::log_breaks
#'
#' @return function
#' @references From \url{https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html} and \url{https://gist.github.com/wch/3250485}
#' @export
#' @examples
#' # mlog_breaks()(runif(1000))
#'

mlog_breaks <- function (n = 5, base = 10)
  {
   # scales:::force_all(n, base)  ## no clue what this does

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
      log_sub_breaks_gggwas(rng, n = n, base = base)
    }
}

#' Minus log transformation
#'
#' @param base base of logarithm to use
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

  #force(base) ## no clue what this does
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
