



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
#' qp <- ggplot(df, aes(observed = P)) +
#'   stat_qqplot() +
#'   geom_abline(intercept = 0, slope = 1)
#' print(qp)
#'
#' ## show only p-values above a cerain threshold
#' ggplot(dat, aes(observed = P)) +
#' stat_qqplot(threshold = 0.05) +
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
#'   stat_qqplot() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#' ## facet
#' ggplot(df, aes(observed = P)) +
#'   facet_wrap(~GWAS) +
#'   stat_qqplot() +
#'   geom_abline(intercept = 0, slope = 1)
#'
#'
#' ## group
#' ggplot(df, aes(observed = P, group = GWAS)) +
#'   stat_qqplot() +
#'   geom_abline(intercept = 0, slope = 1)

stat_qqplot_rastr <- function(mapping = NULL,
                            data = NULL,
                            geom = ggrastr:::GeomPointRast,
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


stat_qqplot <- function(mapping = NULL,
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

stat_qqplot_line <- function(mapping = NULL,
                        data = NULL,
                        geom = "line",
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
  Stat,
  default_aes = aes(y = stat(observed), x = stat(expected)),

  required_aes = c("observed"),

  compute_group = function(data,
                           scales,
                           dparams,
                           na.rm,
                           observed.thresh) {
    if (nrow(data) > 1e5) {
      warning(
        glue::glue(
          "You are plotting {nrow(data)} points. Consider using stat_qqplot_raster()"
        ),
        call. = FALSE
      )
    }

    observed <-
      data$observed#[!is.na(data$x)]
    N <- length(observed)

    ## expected
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


    data.frame(observed, expected)
  }

)


geom_qqplot <- stat_qqplot
