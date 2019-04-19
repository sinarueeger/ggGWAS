# ggGWAS 🚧

An R-Package (*work-in-progress*) that contains ggplot2-extensions of data visualisations used with GWAS data. 

Mainly, these are Q-Q plot and Manhattan plot that both use P-values from GWASs as input. 

An inspiration for ggGWAS has been the R-package [qqman](http://www.gettinggeneticsdone.com/2014/05/qqman-r-package-for-qq-and-manhattan-plots-for-gwas-results.html), except that ggGWAS aims to have the look and functionality of `ggplot2`.

## Installation
```
remotes::install_github("sinarueeger/ggGWAS")
```

Install a`dev`branch:
```
remotes::install_github("sinarueeger/ggGWAS", ref = "dev")
```


Install including vignettes: 
```
remotes::install_github("sinarueeger/ggGWAS", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
vignette("reasoning")
vignette("internals")
```


## Basic usage

```
library(ggGWAS)

## Generate some random data

df <-
  data.frame(
    POS = rep(1:250, 4),
    CHR = 1:4,
    P = runif(1000),
    GWAS = sample(c("a", "b"), 1000, replace = TRUE)
  )
```

### Manhattan plot

```
ggplot(df) + 
stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR))

ggplot(df) + 
stat_gwas_manhattan(aes(pos = POS, y = -log10(P), chr = CHR),  chr.class = "character") + 
facet_wrap( ~ GWAS, label = label_both)

?stat_gwas_manhattan ## for more examples
```

### Q-Q plot
```
ggplot(df) + stat_gwas_qq(aes(observed = P))

ggplot(df) + stat_gwas_qq(aes(observed = P)) + 
facet_wrap( ~ GWAS, label = label_both) + 
geom_abline(intercept = 0, slope = 1)

?stat_gwas_qq ## for more examples
```

