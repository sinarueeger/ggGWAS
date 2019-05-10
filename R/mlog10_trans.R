#' Breaks for -log10 axis
#'
#' @param n number of breaks
#'
#' @return vector
#' @references From \url{https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html} and \url{https://gist.github.com/wch/3250485}
#' @export

mlog10_breaks <- function (n = 5, base = 10)
  {
    scales:::force_all(n, base)

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

#' -log10 transformation
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
#'   scale_y_continuous(trans = mlog10_trans()) +
#'   scale_x_continuous(trans = mlog10_trans()) +
#'   geom_abline(intercept = 0, slope = 1)
mlog10_trans <- function() {

  base <- 10
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)


  scales::trans_new("mlog10",
    transform = trans,
    inverse = inv,
    breaks = mlog_breaks2(base = base),
#    breaks = mlog10_breaks,
    domain = c(1e-300, Inf) ## cause P-values small in GWAS
  )
}
