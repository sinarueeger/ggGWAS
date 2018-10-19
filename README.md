# ggGWAS R package *(work-in-progress)*

An R-Package (soon ;-), currently *work-in-progress*) that implements data visualisations used in GWAS into ggplot2. 

Mainly, these are Q-Q-plots and Manhattanplots that use P-values from GWASs as input. 

{ggGWAS} is similar to the package [{qqman}](http://www.gettinggeneticsdone.com/2014/05/qqman-r-package-for-qq-and-manhattan-plots-for-gwas-results.html), except that it should have the look and functionality of `ggplot2`.

1. [Functionality](#functionality)
2. [Development](#development)
3. [Inspiration](#inspiration)


## Functionality

Here is what the functions should do. 

Let's say we have GWAS summary statistics for a number of SNPs. Let's call this data `gwas.summarystats`: for a number of SNPs (rowwise) we know the SNP identifier (`SNP`) and the P-value (`P`). That would look like this:

```
SNP     P
rs3342  1e-2
rs83    1e-2
...     ...
```

What we want is first, a [Q-Q-plot](https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot) representation of the P-values. Something like this. 

![rplot01](https://user-images.githubusercontent.com/4454726/47022176-323fb600-d15d-11e8-892b-152816ce9574.png)

The ggplot2 code should look ~ like this:

`ggplot(data = gwas.summarystats) + geom_qqplot(aes(y = -log10(P)))`
 
Secondly, we want a [Manhattan plot](https://en.wikipedia.org/wiki/Manhattan_plot).

<p><a href="https://commons.wikimedia.org/wiki/File:Manhattan_Plot.png#/media/File:Manhattan_Plot.png"><img src="https://upload.wikimedia.org/wikipedia/commons/1/12/Manhattan_Plot.png" alt="Manhattan Plot.png" width="640" height="249"></a><br>By M. Kamran Ikram et al - Ikram MK et al (2010) Four Novel Loci (19q13, 6q24, 12q24, and 5q14) Influence the Microcirculation In Vivo. PLoS Genet. 2010 Oct 28;6(10):e1001184. <a href="https://en.wikipedia.org/wiki/Digital_object_identifier" class="extiw" title="w:Digital object identifier">doi</a>:<a rel="nofollow" class="external text" href="https://doi.org/10.1371%2Fjournal.pgen.1001184.g001">10.1371/journal.pgen.1001184.g001</a>, <a href="https://creativecommons.org/licenses/by/2.5" title="Creative Commons Attribution 2.5">CC BY 2.5</a>, <a href="https://commons.wikimedia.org/w/index.php?curid=18056138">Link</a></p>

The ggplot2 code should look ~ like this:

`ggplot(data = gwas.summarystats) + geom_manhattan(aes(x = Pos, y = -log10(P), group = Chr))`


## Development 

There are workarounds how to turn a dataset with GWAS results into something that can be used with `geom_point()`, but this is cumbursome. By writing a ggplot2 extension, we can inherit lots of the default ggplot2 functionalities and shorten the input. 

### ggplot2 extension

How to implement your own geom from 
- [here (vignette)](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html#creating-a-new-geom)
- [R help](https://www.rdocumentation.org/packages/ggplot2/versions/3.0.0/topics/ggplot2-ggproto)
- [here (wiki)](https://github.com/tidyverse/ggplot2/wiki/Creating-a-new-geom) (from 2010, probably obsolete)

There is a [`geom_qq`](https://ggplot2.tidyverse.org/reference/geom_qq.html) in ggplot2 that implements quantile-quantile plots. However, this is not exactly the same as what we want. 

### Testing

How to test plots? 

One option is, to compare ggplot2 object data. In the example below, we are comparing two ggplot2 outputs, one created with `qplot` and one with `ggplot`. 

```
gg1 <- qplot(Sepal.Length, Petal.Length, data = iris)
gg2 <- ggplot(data = iris) + geom_point(aes(Sepal.Length, Petal.Length))
identical(gg1$data, gg2$data)
```
We can apply this to our package by creating the qqplot and manhattanplots manually by hand, and then comparing the to the function outputs.

Another option is to use https://github.com/lionel-/vdiffr

## Inspiration

- http://www.gettinggeneticsdone.com/2014/05/qqman-r-package-for-qq-and-manhattan-plots-for-gwas-results.html
- https://www.r-graph-gallery.com/wp-content/uploads/2018/02/Manhattan_plot_in_R.html

