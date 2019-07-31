## code to prepare `DATASET` dataset goes here

## Source: https://portals.broadinstitute.org/collaboration/giant/index.php/GIANT_consortium_data_files
path_to_file <- "~/tmp/Height_AA_add_SV.txt.gz"


## Height
download.file(
  "https://portals.broadinstitute.org/collaboration/giant/images/8/80/Height_AA_add_SV.txt.gz",
  path_to_file)

height_ <- read.table(path_to_file, header = TRUE)
height <- height_[, c("CHR", "POS", "SNPNAME", "Pvalue")]

usethis::use_data(height, compress = "xz", overwrite = T)

