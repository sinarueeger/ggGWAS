
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
#' @param observed.thresh same scale as observed (e.g. 0.05), observed <= observed.thresh AFTER computing expected
#' @param ...
#' @param bins
#' @param binwidth
#'
#' @return
#' @export
#'
#' @examples
#' require(ggplot2)
#'
#' n.sample <- 1000
#' df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))
#'
#'
#' (qp <- ggplot(df, aes(y = P)) +
#'   stat_qqunif_hex() +
#'   geom_abline(intercept = 0, slope = 1))
#'
#' (qp <- ggplot(df, aes(y = P, group = GWAS, color = GWAS)) +
#'   stat_qqunif_hex() +
#'   geom_abline(intercept = 0, slope = 1))
#'
#'
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
  required_aes = c("y"),
  default_aes = ggplot2::aes(y = stat(y), x = stat(x), weight = 1, fill = stat(count)),

  compute_group = function(data,
                           scales,
                           dparams,
                           na.rm,
                           observed.thresh,
                           binwidth = NULL, bins = 30) {

    observed <-
      data$y#[!is.na(data$x)]
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

    data$y = observed
    data$x = expected
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




geom_qqunif_hex <- stat_qqunif_hex
