#' @title Q-Q plot
#' @description Quantile-quantile plot to compare the p-values of a GWAS to a uniform distribution.
#'
#' @inheritParams ggplot2::geom_point
#' @param observed.thresh same scale as observed (e.g. 0.05), observed <= observed.thresh AFTER computing expected
#' @param geom \code{"point"} by default, \code{"ggrastr:::GeomPointRast"} for a rasterized version.
#'
#' @export
#' @aliases stat_qqunif
#' @details \code{\link[ggplot2]{stat_qq}} works for all kinds of distributions. But using \code{\link[ggplot2]{stat_qq}} with \eqn{-log10()} transformation does not work neatly.
#' @seealso \code{\link[ggplot2]{stat_qq}}, \code{\link{stat_geom_qq_hex}}
#' @note Plotting several thousand points might take time. If you want to speed things up use \code{geom="ggrastr:::GeomPointRast"} or \code{\link{stat_gwas_qq_hex}}.
#' @aliases geom_qq_unif
#'
#' @examples
#' require(ggplot2)
#' n.sample <- 10000
#' df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
#'
#' ## default
#' (qp <- ggplot(df, aes(observed = P)) +
#'   stat_gwas_qq() +
#'   geom_abline(intercept = 0, slope = 1))
#'
#' ## Group points
#' (qp <- ggplot(df, aes(observed = P)) + stat_gwas_qq(aes(group = GWAS, color = GWAS)))
#'
#' ## show only p-values above a cerain threshold
#' ggplot(df, aes(observed = P)) +
#' stat_gwas_qq(observed.thresh = 0.05) +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## plot a line instead
#' ggplot(df, aes(observed = P)) +
#' stat_gwas_qq(geom = "line") +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## plot efficiently
#' ggplot(df, aes(observed = P)) +
#' stat_gwas_qq(geom = ggrastr:::GeomPointRast) +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## adding nice stuff
#' ## identical limits (meaning truely square)
#' qp +
#'   theme(aspect.ratio=1) + ## square shaped
#'   expand_limits(x = -log10(max(df$P)), y = -log10(max(df$P))) +
#'   ggtitle("QQplot") +
#'   xlab("Expected -log10(P)") +
#'   ylab("Observed -log10(P)")
#'
#' ## color
#' ggplot(df, aes(observed = P, color = GWAS)) +
#'   stat_gwas_qq() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#' ## facet
#' ggplot(df, aes(observed = P)) +
#'   facet_wrap(~GWAS) +
#'   stat_gwas_qq() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#'
#' ## group
#' ggplot(df, aes(observed = P, group = GWAS)) +
#'   stat_gwas_qq() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#' ## group
#' library(GWAS.utils) ## devtools::install_github("sinarueeger/GWAS.utils")
#' data("giant")
#' ?giant
#'
#' ## generate two groups
#' giant <- giant %>%
#'   dplyr::mutate(gr = dplyr::case_when(BETA <= 0 ~ "Neg effect size", BETA > 0 ~ "Pos effect size"))
#' ggplot(data = giant, aes(observed = P, group = gr, color = gr)) +
#'   stat_gwas_qq() +
#'   geom_abline(intercept = 0, slope = 1)
#'



stat_gwas_qq <- function(mapping = NULL,
                          data = NULL,
                          geom = "point",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          observed.thresh = NULL,
                          ...) {
    layer(
      stat = StatGwasqqplot,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, observed.thresh = observed.thresh, ...)
    )
  }

#' @export
#' @rdname stat_gwas_qq
geom_gwas_qq <- stat_gwas_qq


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @keywords internal.
StatGwasqqplot <- ggproto(
  "StatGwasqqplot",
  Stat,
  required_aes = c("observed"),
  default_aes = aes(y = stat(`observed_log10`), x = stat(`expected_log10`)),

  compute_group = function(data,
                           scales,
                           dparams,
                           na.rm,
                           observed.thresh) {

    observed <-
      data$observed#[!is.na(data$x)]
    N <- length(observed)


    ## calculate the expected axis
    expected <-
      sort(-log10((1:N) / N - 1 / (2 * N)))
    observed <-
      sort(-log10(observed))

    ## remove points if observed thresh is set.
    if (!is.null(observed.thresh))
    {
      observed.thresh <- -log10(observed.thresh)

      ind <-
        which(observed >= observed.thresh)
      expected <- expected[ind]
      observed <- observed[ind]

    }



    data.frame(`observed_log10` = observed, `expected_log10` = expected)
  }
    #,
  # draw_panels = function(data, panel_scales, coord) {
  #   ## Transform the data first
  #   coords <- coord$transform(data, panel_scales)
  #
  #   ## Let's print out the structure of the 'coords' object
  #   str(coords)
  #
  #   ## Construct a grid grob
  #   pointsGrob(
  #     x = coords$x,
  #     y = coords$y,
  #     pch = coords$shape
  #   )
  # },
  # draw_labels <- function(data, panel_scales, coord) {
  #  has something to do with gtable: https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html
  ## labels from qqman::qq()
  #  xlab(expression(Expected ~ ~-log[10](italic(p)))) +
  #  ylab(expression(Observed ~ ~-log[10](italic(p))))
  #     }

)


