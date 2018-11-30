
## mostly from : https://github.com/tidyverse/ggplot2/blob/master/R/stat-binhex.r


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
#' require(ggplot2)
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
#' ggplot(df, aes(observed = P)) +
#' stat_qqunif(geom = "line") +
#' geom_abline(intercept = 0, slope = 1)
#'
#' ## plot efficiently
#' ggplot(df, aes(observed = P)) +
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



stat_qqunif_hex <- function(mapping = NULL,
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
      stat = StatQQplotHex,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, observed.thresh = observed.thresh,   bins = bins,
                    binwidth = binwidth, ...)
    )
  }


## define the ggproto file
## ------------------
StatQQplotHex <- ggplot2::ggproto(
  "StatQQplotHex",
  ggplot2::Stat,
  required_aes = c("observed"),
  default_aes = ggplot2::aes(y = stat(y), x = stat(x), weight = 1, fill = stat(count)),

  compute_group = function(data,
                           scales,
                           dparams,
                           na.rm,
                           observed.thresh,
                           binwidth = NULL, bins = 30) {

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

    data <- data.frame(x = observed, y = expected)


    try_require("hexbin", "stat_binhex")

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




geom_qqunif <- stat_qqunif
