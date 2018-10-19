
## make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P))
## > should turn this into

## some copied from here:
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r


## real distance
## dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))

## new x axis
## med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
## ------------------




StatManhattan <- ggplot2::ggproto("StatManhattan", Stat,
  compute_group = function(data, scales, params, y.thresh) {

    ## y.thesh: vector
    ## if c(NA, something), then y <= something
    ## if c(something, NA), then y <= something
    ## only include points that are above this threshold
    if (!is.null(y.thresh)) {
      if (!is.na(y.thresh[1])) {
        data <- data %>% filter(y >= y.thresh[1])
      }
      if (!is.na(y.thresh[2])) {
        data <- data %>% filter(y <= y.thresh[2])
      }
    }


    ## equidistance
    data2 <- data %>% dplyr::arrange(chr, pos) %>% dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp))
    ## real distance
    # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))


    ## new x axis
    #                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
    # scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)


    data.frame(x = data2$cumsum.tmp, y = data2$y, colour = as.character(data2$chr))
  },

  required_aes = c("y", "pos", "chr"),
  default_aes = aes(y = stat(y), x = stat(x), colour = stat(colour))
)

#' Manhattan plot with ggplot2 features
#'
#' @param mappingd
#' @param data
#' @param geom
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param y.thresh a vector
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' dat <- qqman::gwasResults# %>% filter(P < 0.05)#, chr="CHR", bp="BP", snp="SNP", p="P" )
#'
#'
#' qp <- ggplot(dat %>% mutate(CHR2 = as.character(CHR))) +
#'  stat_manhattan(aes(pos = BP, y = -log10(P), chr = CHR)) +
#'  geom_hline(yintercept = 8) +
#'  ggtitle("sfsdfsdf")
#' print(qp)
#'
#'
stat_manhattan <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, y.thresh = NULL, ...) { # , dparams = list()
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      y.thresh = y.thresh,
      ...
    )
  )
}

stat_manhattan_rastr <- function(mapping = NULL, data = NULL, geom = ggrastr:::GeomPointRast,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, y.thresh = NULL, ...) { # , dparams = list()
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      y.thresh = y.thresh,
      ...
    )
  )
}

geom_manhattan <- stat_manhattan
