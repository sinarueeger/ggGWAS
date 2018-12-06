
#' Theme GWAS (just a random theme, nothing really gwas specific)
#'
#' @inheritParams ggplot2::theme_gray
#'
#' @details Makes a few modifications to \code{\link{theme_gray}}. Similar to \code{\link{theme_bw}}.
#' @return Returns an object of class theme and gg.
#' @export
#'
#' @examples
#' require(ggplot2)
#' qplot(Sepal.Length, Sepal.Width, data = iris) + theme_gwas()
#'
theme_gwas <- function(...) {

  theme_grey(...) %+replace%
    theme(
      strip.text.y = element_text(angle = 0), panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"), panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_line(colour = "grey92", size = 0.25), strip.background = element_rect(fill = "grey85", colour = "grey20"),
      legend.key = element_rect(fill = "white", colour = NA), complete = TRUE
    )
}
