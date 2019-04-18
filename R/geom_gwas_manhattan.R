
#' @title Manhattan plot
#' @description Manhattan plot for GWAS data
#'
#' @inheritParams ggplot2::geom_point
#' @param y.thresh cutoff for y-axis, defined as a vector of length two. If \code{c(K, NA)}, points with a y-value lower than K will be removed. If \code{c(NA, K)} points with y-values larger than K will be removed.
#' @param chr.class Relevant for coloring of the points: what class the chromosomes should be represented as. If "numeric", coloring will be continuous. If "character" coloring will be discrete.
#' @param geom \code{"point"} by default
#' @return sdfsdf
#' @export
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Stat

#' @details See also \url{https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r}
#'
#' @examples
#' require(ggplot2)
#' require(GWAS.utils) ## devtools::install_github("sinarueeger/GWAS.utils")
#' data("giant")
#' ?giant
#' theme_set(theme_gwas())
#'
#' ## default: for -log10(P), by default chr is numeric
#' qp <- ggplot(giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR)) +
#'   geom_hline(yintercept = 8) +
#'   ggtitle("GIANT summary statistics (by default CHR is numeric)")
#' print(qp)
#'
#' ## add nice color palette
#' pal <- wesanderson::wes_palette("Zissou1", 22, type = "continuous")
#' qp + scale_color_gradientn(colours = pal)
#'
#' ## chr factor
#' qp <- ggplot(giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), chr.class = "character") +
#'   geom_hline(yintercept = 8) +
#'   ggtitle("GIANT summary statistics (CHR is now a character/factor)")
#' print(qp)
#' ## adding a nice color palette
#' qp + scale_color_manual(values = wesanderson::wes_palette("Zissou1", 22, type = "continuous"))
#'
#' ## turn all points black
#' qp <- ggplot(giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), color = "black", alpha = I(0.4)) +
#'   geom_hline(yintercept = 8) +
#'   ggtitle("GIANT summary statistics")
#' print(qp)
#'
#' ## set lower threshold
#' qp <- ggplot(data = giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), y.thresh= c(2, NA)) +
#'   geom_hline(yintercept = 8) +
#'   ggtitle("GIANT summary statistics")
#' print(qp)
#'
#'
#' ## for effect sizes
#' qp <- ggplot(data = giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = BETA, chr = CHR)) +
#'   ggtitle("GIANT effect sizes")
#' print(qp)
#'
#' ## use rastr
#' qp <- ggplot(data = giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), geom = ggrastr:::GeomPointRast) +
#'   geom_hline(yintercept = 8) +
#'   ggtitle("GIANT summary statistics (rastr)")
#' print(qp)
#'
#' ## facetting
#'
#' ## generate two groups
#' giant <- giant %>%
#'   dplyr::mutate(gr = dplyr::case_when(BETA <= 0 ~ "Neg effect size", BETA > 0 ~ "Pos effect size"))
#' qp <- ggplot(data = giant) +
#'   stat_gwas_manhattan(aes(pos = POS, y = BETA, chr = CHR)) +
#'   ggtitle("GIANT summary statistics") +
#'   facet_wrap(~gr)
#' print(qp)


stat_gwas_manhattan <-
  function(mapping = NULL,
           data = NULL,
           geom = "point",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           y.thresh = NULL,
           chr.class = "numeric",
           ...) {
    # , dparams = list()
    layer(
      stat = StatGwasManhattan,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        y.thresh = y.thresh,
        chr.class = chr.class,
        ...
      )
    )
  }

#' @export
#' @rdname stat_gwas_manhattan
geom_gwas_manhattan <- stat_gwas_manhattan



#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatGwasManhattan <- ggproto(
  "StatGwasManhattan",
  Stat,
  required_aes = c("y", "pos", "chr"),
  default_aes = aes(
    y = stat(y),
    x = stat(`Pos`),
    colour = stat(colour)
  ),
  compute_group = function(data, scales, params, y.thresh, chr.class) {
    ## chr.class can be "numeric" or "factor"
    ## y.thesh: vector
    ## if c(NA, something), then y <= something
    ## if c(something, NA), then y <= something
    ## only include points that are above this threshold

    if (!is.null(y.thresh)) {
    ## try to solve this with ylim
      if (!is.na(y.thresh[1])) {
        data <- data %>% dplyr::filter(y >= y.thresh[1])
      }
      if (!is.na(y.thresh[2])) {
        data <- data %>% dplyr::filter(y <= y.thresh[2])
      }
    }


    ## equidistance
    data2 <- data %>% dplyr::arrange(chr, pos) %>% dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp))


    ## real distance
    # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
    ## new x axis
    #                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
    # scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)


    class(data2$chr) <- chr.class

    ## stupid hack
    if (chr.class == "character") {
      data2$chr <- as.factor(as.numeric(data2$chr))
    }

    data.frame(`Pos` = data2$cumsum.tmp,
               y = data2$y,
               colour = data2$chr)
  }
)
