


#' QQ-plot
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param method point or raster
#' @param observed.thresh same scale as observed (e.g. 0.05), observed <= observed.thresh AFTER computing expected
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' n.sample <- 10000
#' df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
#'
#' ## default
#' (qp <- ggplot(df, aes(observed = P)) +
#'   stat_qqunif() +
#'   geom_abline(intercept = 0, slope = 1))
#'
#' ## Group points
#' (qp <- ggplot(df, aes(observed = P)) + stat_qqunif(aes(group = GWAS, color = GWAS)))
#'
#' ## show only p-values above a cerain threshold
#' ggplot(df, aes(observed = P)) +
#' stat_qqunif(observed.thresh = 0.05) +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## plot a line instead
#' ggplot(dat, aes(observed = P)) +
#' stat_qqunif(geom = "line") +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## plot efficiently
#' ggplot(dat, aes(observed = P)) +
#' stat_qqunif(geom = ggrastr:::GeomPointRast) +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## adding nice stuff
#' qp +
#'   theme(aspect.ratio=1) + ## square shaped
#'   expand_limits(x = -log10(max(df$P)), y = -log10(max(df$P))) + ## identical limits (meaning truely square)
#'   ggtitle("QQplot") + ## title
#'   xlab("Expected -log10(P)") + ## axis labels
#'   ylab("Observed -log10(P)")
#'
#' ## color
#' ggplot(df, aes(observed = P, color = GWAS)) +
#'   stat_qqunif() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#' ## facet
#' ggplot(df, aes(observed = P)) +
#'   facet_wrap(~GWAS) +
#'   stat_qqunif() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#'
#' ## group
#' ggplot(df, aes(observed = P, group = GWAS)) +
#'   stat_qqunif() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#' ## group
#' library(GWAS.utils) ## devtools::install_github("sinarueeger/GWAS.utils")
#' data("giant")
#' ?giant
#'
#' giant <- giant %>% dplyr::mutate(gr = dplyr::case_when(BETA <= 0 ~ "Neg effect size", BETA > 0 ~ "Pos effect size"))## generate two groups
#' ggplot(data = giant, aes(observed = P, group = gr, color = gr)) +
#'   stat_qqunif() +
#'   geom_abline(intercept = 0, slope = 1)
#'



stat_qqunif <- function(mapping = NULL,
                          data = NULL,
                          geom = "point",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          observed.thresh = NULL,
                          ...) {
    layer(
      stat = StatQQplot,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, observed.thresh = observed.thresh, ...)
    )
  }


## define the ggproto file
## ------------------
StatQQplot <- ggplot2::ggproto(
  "StatQQplot",
  ggplot2::Stat,
  required_aes = c("observed"),
  default_aes = ggplot2::aes(y = stat(`observed_log10`), x = stat(`expected_log10`)),

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


    if (length(expected) > 1e5) {
      message(
        glue::glue(
          "You are plotting {length(expected)} points. Consider using stat_qqunif_raster (if you are not using it already)."
        ),
        call. = FALSE
      )
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



geom_qqunif <- stat_qqunif
