###########################################################################
###########################################################################
###                                                                     ###
###                   COPIED FROM GGPLOT2 FOR TESTING                   ###
###                                                                     ###
###########################################################################
###########################################################################

## copied from github website
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-binhex.r


stat_bin_hex <- function(mapping = NULL, data = NULL,
                         geom = "hex", position = "identity",
                         ...,
                         bins = 30,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBinhex,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_hex
#' @usage NULL
stat_binhex <- stat_bin_hex

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBinhex <- ggproto("StatBinhex", Stat,
                      default_aes = aes(weight = 1, fill = stat(count)),

                      required_aes = c("y"),

                      compute_group = function(data, scales, binwidth = NULL, bins = 30,
                                               na.rm = FALSE) {


                        observed <-
                          data$y#[!is.na(data$x)]
                        N <- length(observed)


                        ## calculate the expected axis
                        expected <-
                          sort(-log10((1:N) / N - 1 / (2 * N)))
                        observed <-
                          sort(-log10(observed))


                        data$x <- expected
                        data$y <- observed

                        #try_require("hexbin", "stat_binhex")
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

