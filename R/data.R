#' Height GWAS
#'
#' Associations between human body height and genetic markers (exome array)
#' in African American population (n ~ 27K).
#'
#' @format The data set contains 4 variables (the original dataset contains 10 variables):
#' \describe{
#' \item{\code{CHR}}{Chromosome}
#' \item{\code{POS}}{Physical position (Genome build hg19)}
#' \item{\code{snp}}{SNP identifier}
#' \item{\code{p}}{P-value of the effect size}
#' }
#'
#' @details The summary statistics by running a genome-wide association study (GWAS) for
#'  height in ~27K individuals of African American ancestry. Each line in the dataset corresponds
#'  to the association between height (transformed) as an outcome and one genetic marker
#'  (indicated in \code{snp}) plus covariates as predictors.
#'
#' @source
#' Data: \url{https://portals.broadinstitute.org/collaboration/giant/images/8/80/Height_AA_add_SV.txt.gz}
#' Data description: \url{https://portals.broadinstitute.org/collaboration/giant/images/2/29/Data_File_Description_2018_Exome_Array_Summary_Statistics_Height_Exome_Array_Summary_Statistics.docx}
"height"
