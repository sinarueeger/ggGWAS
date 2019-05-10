# install.packages("devtools")
# install.packages("usethis")

usethis::use_description(fields = list(
  Title = "ggplot2 Extensions for Plotting GWAS Summary Statistics",
  `Authors@R` = 'person("Sina", "Rüeger",
                                       email = "sina.rueeger@gmail.com",
                                       role = c("aut", "cre"))',
  Description = "Provides Manhattan plot and QQplot as a ggplot2 extension. For high-dimensional GWAS data there is a hex version of the QQplot (which is more of a diagnostics tool)."
))

usethis::use_github_links()
usethis::use_version(which = "patch") ## "major", "minor", "patch", "dev"

# initial setup
usethis::use_data_raw()
# usethis::use_cran_badge()
usethis::use_mit_license("Sina Rüeger")
usethis::use_readme_rmd()

## create `testthat` folder
usethis::use_testthat()

## add Roxygen into description file
usethis::use_roxygen_md()

## Suggest & Imports
## adds suggests and imports to DESCRIPTION
usethis::use_package("ggplot2", type = "Import", min_version = "3.1.0")
usethis::use_package("dplyr", type = "Import", min_version = "0.7.8")
usethis::use_package("scales", type = "Import", min_version = "1.0.0")
usethis::use_package("hexbin", type = "Import", min_version = "1.27.2")

# usethis::use_package("ggrastr", type = "Suggests", min_version = "0.1.7" )
# usethis::use_package("GWAS.utils", type = "Remotes", min_version = "0.0.0.9000" )
usethis::use_package("skimr", type = "Suggests")
usethis::use_package("plyr", type = "Suggests", min_version = "1.8.4")
usethis::use_package("wesanderson", type = "Suggests", min_version = "0.3.6")
usethis::use_package("vctrs", type = "Suggests", min_version = "0.1.0")
usethis::use_package("knitr", type = "Suggests", min_version = "0.1.7")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("qqman", type = "Suggests")
usethis::use_package("pryr", type = "Suggests")

## manually adding remote
# https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
## add to
## Remotes: sinarueeger/GWAS.utils,

usethis::use_pipe()

# Vignettes
usethis::use_vignette("gggwas-reasoning", "ggGWAS reasoning")
usethis::use_vignette("gggwas-internals", "ggGWAS internals")

## Build site, and make pkgdown
devtools::document()
devtools::build_vignettes()
devtools::build()
usethis::use_package_doc()
codemetar::write_codemeta()

## CI
usethis::use_travis()
usethis::use_coverage(type = c("codecov"))

## run examples
devtools::run_examples()
