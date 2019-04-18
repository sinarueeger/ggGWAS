# ggGWAS 🚧

An R-Package (*work-in-progress*) that contains ggplot2-extensions of data visualisations used with GWAS data. 

Mainly, these are Q-Q plot and Manhattan plot that both use P-values from GWASs as input. 

An inspiration for ggGWAS has been the R-package [qqman](http://www.gettinggeneticsdone.com/2014/05/qqman-r-package-for-qq-and-manhattan-plots-for-gwas-results.html), except that ggGWAS aims to have the look and functionality of `ggplot2`.

## Installation
```
remotes::install_github("sinarueeger/ggGWAS")
```
or (if you are courageous) load a specific branch:
```
remotes::install_github("sinarueeger/ggGWAS", ref = "[BRANCH]")
```


## Basic usage

```
## Random data --------------------

df <-
  data.frame(
    POS = rep(1:250, 4),
    CHR = 1:4,
    P = runif(1000),
    GWAS = sample(c("a", "b"), 1000, replace = TRUE)
  )


## Q-Q plot --------------------

ggplot(df, aes(observed = P)) + ggGWAS::stat_qgwas_manhattanaes(group = GWAS, color = GWAS))


## Manhattan plot --------------------

ggplot(data = df) +
  ggGWAS::stat_mgwas_anhattan(aes(
    pos = POS,
    y = -log10(P),
    chr = CHR
  ),  chr.class = "character") +
  facet_wrap( ~ GWAS)

```


To install the package *including* the vignette, use the following command:

```
remotes::install_github("sinarueeger/ggGWAS", build = TRUE, 
build_opts = c("--no-resave-data", "--no-manual")
```

Then look at the vignette with `vignette("gggwas-package")`.
