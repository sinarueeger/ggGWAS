#' @title Efficient Q-Q plot
#' @description Density-like QQ-plot with hexagons for high-dimensional data.
#'
#'
#' @inheritParams ggplot2::stat_bin_hex
#' @param observed.thresh Same scale as observed (e.g. 0.05),
#' observed <= observed.thresh AFTER computing expected.
#' @details Code and documentation mostly from
#' \url{https://github.com/tidyverse/ggplot2/blob/master/R/stat-binhex.r}.
#' @seealso \code{\link[ggplot2]{stat_bin_hex}}
#' @export
#'
#' @examples
#' require(ggplot2)
#'
#' n.sample <- 1000
#' df <- data.frame(P = runif(n.sample), GWAS = sample(c("a", "b"),
#'   n.sample,
#'   replace = TRUE
#' ))
#'
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
                             observed.thresh = NULL,
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
      observed.thresh = observed.thresh,
      bins = bins,
      binwidth = binwidth, ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatGwasQqplotHex <- ggproto(
  "StatGwasQqplotHex",
  Stat,
  required_aes = c("y"),
  default_aes = aes(y = stat(y), x = stat(x), weight = 1, fill = stat(count)),

  compute_group = function(data,
                             scales,
                             dparams,
                             na.rm,
                             observed.thresh,
                             binwidth = NULL, bins = 30) {
    observed <-
      data$y # [!is.na(data$x)]
    N <- length(observed)


    ## calculate the expected axis
    expected <-
      sort(-log10((1:N) / N - 1 / (2 * N)))
    observed <-
      sort(-log10(observed))

    ## remove points if observed thresh is set.
    if (!is.null(observed.thresh)) {
      observed.thresh <- -log10(observed.thresh)

      ind <-
        which(observed >= observed.thresh)
      expected <- expected[ind]
      observed <- observed[ind]
    }

    data$y <- observed
    data$x <- expected

    # try_require("hexbin", "stat_binhex")
    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    wt <- data$weight %||% rep(1L, nrow(data))
    out <- hexBinSummarise(data$x, data$y, wt, binwidth, sum)

    out$density <- as.vector(out$value / sum(out$value, na.rm = TRUE))
    out$ndensity <- out$density / max(out$density, na.rm = TRUE)
    out$count <- out$value
    out$ncount <- out$count / max(out$count, na.rm = TRUE)
    out$value <- NULL

    out
  }
)

#' @export
#' @rdname stat_gwas_qq_hex
geom_gwas_qq_hex <- stat_gwas_qq_hex
