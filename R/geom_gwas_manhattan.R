
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


stat_gwas_manhattan <- function() {


  .Defunct(msg = "The functions geom_gwas_manhattan and stat_gwas_manhattan are under construction.

Please use ggman::ggmanhattan() from https://github.com/mkanai/ggman.

devtools::install_github('mkanai/ggman')

Here is an example:

library(ggplot2)
library(dplyr)
library(GWAS.utils) ##  devtools::install_github('sinarueeger/ggGWAS')
library(ggman)
ggman::ggmanhattan(data = giant, SNP = 'SNP', chr = 'CHR',
bp = 'POS', P = 'P', sparsify = FALSE,
theme_base = ggplot2::theme_bw(), build = 'hg18',
highlight = giant %>% slice(which.min(P)) %>% pull(SNP)) +
labs(title = 'MHTPLOT')")

  }

#' @export
#' @rdname stat_gwas_manhattan
geom_gwas_manhattan <- stat_gwas_manhattan

