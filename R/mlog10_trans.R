#' Breaks for -log10 axis
#'
#' @param n number of breaks
#'
#' @return vector
#' @references From \url{https://stat.ethz.ch/pipermail/r-help/2014-April/373728.html} and \url{https://gist.github.com/wch/3250485}
#' @export
mlog10_breaks <- function(n = 10) {
  function(x) as.numeric(scales::pretty_breaks(n)(x))
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
  scales::trans_new("-log10",
    transform = function(x) -log10(x),
    inverse = function(x) 10^(-x),
    breaks = mlog10_breaks()
  )
}
