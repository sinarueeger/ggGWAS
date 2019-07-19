
#' @title Manhattan plot
#' @description Manhattan plot for GWAS data.
#'
#' @details The functions geom_gwas_manhattan and stat_gwas_manhattan are under construction. Use ggman::ggmanhattan() from https://github.com/mkanai/ggman in the meantime. \code{devtools::install_github('mkanai/ggman')}.
#'
#' @return ggplot2 object
#' @export
#' @aliases geom_gwas_manhattan
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(GWAS.utils) ## devtools::install_github("sinarueeger/GWAS.utils")
#' library(ggman)      ## devtools::install_github("mkanai/ggman")
#' data("giant")
#' ggman::ggmanhattan(data = giant, SNP = "SNP", chr = "CHR", bp = "POS",
#'                    P = "P", sparsify = FALSE,
#'                    theme_base = ggplot2::theme_bw(), build = 'hg18')
#'
#' ggman::ggmanhattan(data = giant, SNP = "SNP", chr = "CHR", bp = "POS",
#'                    P = "P", sparsify = FALSE,
#'                    theme_base = ggplot2::theme_bw(), build = 'hg18',
#'                    scale_color = ggman::scale_colour_traditional())
#'
#' ggman::ggmanhattan(data = giant, SNP = "SNP", chr = "CHR", bp = "POS",
#'                    P = "P", sparsify = FALSE,
#'                    theme_base = ggplot2::theme_bw(), build = 'hg18',
#'                    highlight = giant %>% slice(which.min(P)) %>% pull(SNP)) +
#'                    labs(title = "MHTPLOT" )
#' }

stat_gwas_manhattan <- function() {


  .Defunct(msg = "The functions geom_gwas_manhattan and stat_gwas_manhattan are under construction.\n\nPlease use ggman::ggmanhattan() from https://github.com/mkanai/ggman.\n\ndevtools::install_github('mkanai/ggman')")

  }

#' @export
#' @rdname stat_gwas_manhattan
geom_gwas_manhattan <- stat_gwas_manhattan

