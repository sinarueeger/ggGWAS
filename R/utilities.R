## from: https://rdrr.io/github/hadley/ggplot2/src/R/hexbin.R


hex_binwidth <- function(bins = 30, scales) {
  c(
    diff(scales$y$dimension()) / bins, ## tweaked
    diff(scales$y$dimension()) / bins
  )
}

hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

hexBinSummarise <- function(x, y, z, binwidth, fun = mean, fun.args = list(), drop = TRUE) {
  if (length(binwidth) == 1) {
    binwidth <- rep(binwidth, 2)
  }

  # Convert binwidths into bounds + nbins
  xbnds <- hex_bounds(x, binwidth[1])
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- hex_bounds(y, binwidth[2])
  ybins <- diff(ybnds) / binwidth[2]

  # Call hexbin
  hb <- hexbin::hexbin(
    x, xbnds = xbnds, xbins = xbins,
    y, ybnds = ybnds, shape = ybins / xbins,
    IDs = TRUE
  )

  value <- do.call(tapply, c(list(quote(z), quote(hb@cID), quote(fun)), fun.args))

  # Convert to data frame
  out <- vctrs::new_data_frame(hexbin::hcell2xy(hb))
  out$value <- as.vector(value)

  if (drop) out <- stats::na.omit(out)
  out
}



##copied from: https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n",
       "Please install and try again.", call. = FALSE)
}
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

