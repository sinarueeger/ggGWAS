#' @title Efficient Q-Q plot
#' @description Density-like QQ-plot with hexagons for high-dimensional data.
#'
#'
#' @inheritParams ggplot2::stat_bin_hex
#' @param y.thresh Same scale as y (e.g. 0.05),
#' y <= y.thresh AFTER computing expected.
#' @param fill color by which hexagons are filled, by default black.
#' @param hex.function \code{ggGWAS:::hexBinSummarise} or \code{ggplot2:::hexBinSummarise}
#' @details Code and documentation mostly from
#' \url{https://github.com/tidyverse/ggplot2/blob/master/R/stat-binhex.r}.
#' @seealso \code{\link[ggplot2]{stat_bin_hex}}
#' Variables computed by `stat_gwas_qq_hex`:
#' \describe{
#'   \item{y}{Observed P-value quantiles}
#'   \item{x}{Expected/theoretical quantiles}
#' }
#' @export
#' @aliases geom_gwas_qq_hex
#'
#' @examples
#' require(ggplot2)
#'
#' n.sample <- 1000
#' df <- data.frame(P = runif(n.sample), GWAS = sample(c("a", "b"),
#'   n.sample,
#'   replace = TRUE
#' ))
#' theme_set(theme_bw())
#'
#' (qp <- ggplot(df, aes(y = P)) +
#'   stat_gwas_qq_hex() +
#'   geom_abline(intercept = 0, slope = 1))
#'
#' (qp <- ggplot(df, aes(y = P, group = GWAS, color = GWAS)) +
#'   stat_gwas_qq_hex() +
#'   geom_abline(intercept = 0, slope = 1))
stat_gwas_qq_hex <- function(mapping = NULL,
                             data = NULL,
                             geom = "hex",
                             position = "identity",
                             na.rm = FALSE,
                             bins = 30,
                             binwidth = NULL,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             y.thresh = NULL,
                             hex.function = hexBinSummarise,
                             fill = "black",
                             ...) {
  layer(
    stat = StatGwasQqplotHex,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      y.thresh = y.thresh,
      bins = bins,
      binwidth = binwidth,
      hex.function = hex.function,
      fill = fill,
      ...
    )
  )
}


#' @rdname ggGWAS-ggproto
#' @format NULL
#' @usage NULL
# #' @export
#' @keywords internal

StatGwasQqplotHex <- ggproto(
  "StatGwasQqplotHex",
  Stat,
  default_aes = aes(y = stat(y), x = stat(x), weight = 1),
  required_aes = c("y"),

  compute_group = function(data,
                             scales,
                             dparams,
                             na.rm,
                             y.thresh,
                             binwidth = NULL, bins = 30,  fill = "black", hex.function = hexBinSummarise) {
    # browser()
    observed <-
      data$y#[!is.na(data$obs)]
    N <- length(observed)


    check_range_pvalues(observed)


    ## calculate the expected axis
    expected <-
      sort(-log10((1:N) / N - 1 / (2 * N)))
    observed <-
      sort(-log10(observed))

    ## remove points if observed thresh is set.
    if (!is.null(y.thresh)) {
      y.thresh <- -log10(y.thresh)

      ind <-
        which(observed >= y.thresh)
      expected <- expected[ind]
      observed <- observed[ind]
    }

    data$y <- observed
    data$x <- expected

    # try_require("hexbin", "stat_binhex")
    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    wt <- data$weight %||% rep(1L, nrow(data))
    out <- hex.function(data$x, data$y, wt, binwidth, sum)

    ## no color needed
    # out$density <- as.vector(out$value / sum(out$value, na.rm = TRUE))
    # out$ndensity <- out$density / max(out$density, na.rm = TRUE)
    # out$count <- out$value
    # out$ncount <- out$count / max(out$count, na.rm = TRUE)

    out$value <- NULL
    out$fill <- fill
    out

  }
)

#' @export
#' @rdname stat_gwas_qq_hex
geom_gwas_qq_hex <- stat_gwas_qq_hex


