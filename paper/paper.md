---
title: 'ggGWAS: A ggplot2 extension for genomic summary statistics'
tags:
  - R
  - visualisation
  - human genomics
authors:
  - name: Sina Rüeger
    orcid: 0000-0003-2848-9242
    affiliation: "1, 2" 
  - name: Author 2
    orcid: 0000-0000-0000-0000
    affiliation: 2
affiliations:
 - name: Global Health Institute, School of Life Sciences, EPFL, Lausanne, Switzerland
   index: 1
 - name: Swiss Institute of Bioinformatics, Lausanne, Switzerland
   index: 2
date: 30 August 2019
bibliography: paper.bib
output: 
  md_document:
    preserve_yaml: true
---

<!----
output:
  md_document:
    preserve_yaml: true
------>

Summary
=======

Genome-wide association studies (GWAS) estimate the association between
each genetic variant available (e.g. single nucleotide variant (SNV),
often &gt; 1 Mio genetic variants) and a trait of interest. To run a
GWAS, command line tools are used (e.g. Chang et al. (2015) or Yang et
al. (2011)) that will output the results in form of a text file
containing the summary statistics. The output of a GWAS is a matrix that
contains the information and summary statistic for each genetic variant
per line. The columns include the SNP identifier, chromosome, position,
effect size, standard error, p-value, effect allele, and sometimes the
minor allele frequency.

The exploration of the results is done in conventional statistical
software, such as R (R Core Team 2018). To summarise such large amounts
of data and verify model assumptions two plots are inspected:

-   A Q-Q plot displays the theoretical against the observed p-values
    (McCarthy et al. 2008). This helps to identify inflated P-values due
    to a mispecified GWAS model or any other deviation from the uniform
    distribution.  
-   A Manhattan plot displays the p-values along the chromosomal
    position (McCarthy et al. 2008) and aims to show the accumulation of
    potential hits accross the genome in a somewhat spatial manner.

![An illustration of a Q-Q plot (LHS) and Manhattan plot
(RHS)](illustration.jpeg)

Q-Q plots are an important tool during the analysis process, but a
Manhattan plot can become a primary figure of an article. For example, a
recent GWAS in anorexia (Watson et al. 2019) shows a Manhattanplot as
Figure 1, but the Q-Q plot is part of the Supplementary material.
Therefore, fast plotting is key to quickly summarise results during the
analysis process and take decisions, especially when multiple GWAS are
analysed at the same time (Ansari et al. 2017). Equally important is
versatile plotting - being able to add layers and annotation to a plot
to make it easily understandable and visually appealing for the reader
of a manuscript.

`ggplot2`(Wickham 2016) offers such a versatile data visualisation
package that is widely used. However, `ggplot2` is not optimised for
plotting high dimensional data.

With `ggGWAS` we propose a `ggplot2` extension for GWAS Q-Q plots
(`geom_gwas_qq`) and Manhattan plots (`geom_gwas_manhattan`). For
`geom_qqplot` there is additionally a hexagon version that reduces
rendering speed by xx (`geom_gwas_qq_hex`). For both functions there is
a filtering argument where points can be excluded from plotting, which
reduces both, the rendering speed and object size in R.

Background
==========

Q-Q plots and Manhattan plots are implemented in R with various
functions. The most widely used package is `qqman` (Turner 2017), that
has functions based on base R `graphics` (`qq` and `manhattan`).
Similarly, the `ramwas` bioconductor package (Shabalin et al. 2018) has
two fast plotting functions based on baseR too (`manPlotFast` and
`qqPlotFast`). There are many `ggplot2` wrapper, e.g. `mkanai/ggman`
(Kanai 2015), `drveera/ggman` (Rajagopal 2016), `ggbio` (Yin, Cook, and
Lawrence 2012), but none of these is an extension that inherits the
ggplot2 functionalities.

How can we speed up plotting with `ggplot2`? With one approach, we can
plot a subset of points (called *filtering* later). Another approach
uses *hexagons* to represent neighbouring points. Hexagons are
implemented in R and `ggplot2` and have been used by others to plot high
dimensional data (Freytag 2019). *Filtering* reduces the `ggplot2`
object size and the rendering speed, while the use of *hexagons* reduces
the rendering speed.

<!---------
There is a third option which we will call "rastering", but this will only affect the resulting image file size and not really the plotting speed in R.

|           | Obj Size | Plotting speed | File size |
|-----------|----------|----------------|-----------|
| Filtering | x        | x              | x         |
| hexagons    |          | x              | x         |
| raster    |          |                | x         |
-------->

The `ggGWAS` R package
======================

The `ggGWAS` package depends mainly on `ggplot2`, `hexbin`, `scales`.

Brief example
=============

Below is a brief example with real exome based summary statistics from
the [Genetic Investigation of ANthropometric
Traits](https://portals.broadinstitute.org/collaboration/giant/index.php/GIANT_consortium_data_files).

Data
----

    data <- vroom::vroom(
      c(path_to_file_1, path_to_file_2), id = "path") %>% 
      dplyr::mutate(gwas = 
               dplyr::case_when(path == "~/tmp/BMI_African_American.fmt.gzip"  ~ "BMI-AA",
                         path == "~/tmp/Height_AA_add_SV.txt.gz"  ~ "Height-AA"
                         )
             )

    data %>% janitor::tabyl(gwas)

    ##       gwas      n percent
    ##     BMI-AA 246328     0.5
    ##  Height-AA 246328     0.5

    data_height <- data %>% dplyr::filter(gwas == "Height-AA")
    data_height[1:3,]

    ## # A tibble: 3 x 12
    ##   path  CHR      POS REF   ALT   SNPNAME AFR_MAF ExAC_AFR_MAF   beta    se
    ##   <chr> <chr>  <dbl> <chr> <chr> <chr>   <chr>   <chr>         <dbl> <dbl>
    ## 1 ~/tm… 1     1.00e8 C     A     rs7543… A:0.22… -            -0.017 0.011
    ## 2 ~/tm… 1     1.00e8 G     C     rs1439… C:0     T:0,C:0.000… -0.32  0.18 
    ## 3 ~/tm… 1     1.00e8 C     A     rs1180… A:0.07… A:0.06775    -0.011 0.018
    ## # … with 2 more variables: Pvalue <dbl>, gwas <chr>

Q-Q plot
--------

To create a Q-Q plot we can use the function `geom_gwas_qq`. As input we
pass on the p-values to the aesthetics `y`. The `geom_gwas_qq` function
will then compute the expected -log10(p-value) and display them on the
x-axis, while the observed -log10(p-values) are shown on the y-axis. To
exclude p-values above a certain threshold from the graph, set
`y.thresh`[1].

<img src="paper_files/figure-markdown_strict/qqplot-example-1.png" width="40%" />

To enable hexagons, we can simply add `_hex` to the function
`geom_gwas_qq`. Note, that by default, `geom_gwas_qq_hex` does not use
the default hexagons `ggplot2:::hexBinSummarise`, but a tweaked version
(`ggGWAS:::hexBinSummarise`).

<img src="paper_files/figure-markdown_strict/qqplot-example-hex-1.png" width="40%" />

Manhattanplot
-------------

`ggplot(data = giant) + geom_gwas_manhattan(aes(pos = POS, chr = CHR, y = -log10(P)))`

-   range of chromosomes can be passed on
-   allow for the `raster` version (for faster plotting) and Pvalue
    thresholding (removing the high Pvalue SNPs from the plot)
-   implement coloring (two alternating colors)

Discussion
==========

-   even though facetting could work, this is probably not
    computationally possible (RAM!) when multiple GWAS with millions of
    SNPs are present.

-   implemented rastering, but that did not work (only dcreased file
    size)

-   speed comparison
    `qqman::manhattan(giant, chr = "CHR" , bp = "POS", p = "P")`

Acknowledgements
================

<!-------- We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong ----->

-   Flavia Hodel contributed parts of the Manhattan plot
-   Fellay lab

References
==========

Ansari, M. Azim, Vincent Pedergnana, Camilla L. C. Ip, Andrea Magri,
Annette Von Delft, David Bonsall, Nimisha Chaturvedi, et al. 2017.
“Genome-to-Genome Analysis Highlights the Effect of the Human Innate and
Adaptive Immune Systems on the Hepatitis C Virus.” *Nature Genetics* 49
(5): 666–73. <https://doi.org/10.1038/ng.3835>.

Chang, Christopher C., Carson C. Chow, Laurent CAM Tellier, Shashaank
Vattikuti, Shaun M. Purcell, and James J. Lee. 2015. “Second-Generation
PLINK: Rising to the Challenge of Larger and Richer Datasets.”
*GigaScience* 4 (1): 7. <https://doi.org/10.1186/s13742-015-0047-8>.

Freytag, Saskia. 2019. *Hexbin Plots for Single Cell Omics Data*.
<https://github.com/SaskiaFreytag/schex>.

Kanai, Masahiro. 2015. *ggman: Manhattan Plot Using Ggplot2*.
<https://github.com/mkanai/ggman>.

McCarthy, Mark I., Gonçalo R. Abecasis, Lon R. Cardon, David B.
Goldstein, Julian Little, John P. A. Ioannidis, and Joel N. Hirschhorn.
2008. “Genome-Wide Association Studies for Complex Traits: Consensus,
Uncertainty and Challenges.” *Nature Reviews Genetics* 9 (5): 356–69.
<https://doi.org/10.1038/nrg2344>.

Rajagopal, Veera M. 2016. *ggman: R Package for Manhattan Plots*.
<https://github.com/drveera/ggman>.

R Core Team. 2018. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Shabalin, Andrey A, Mohammad W Hattab, Shaunna L Clark, Robin F Chan,
Gaurav Kumar, Karolina A Aberg, and Edwin JCG van den Oord. 2018.
“RaMWAS: Fast Methylome-Wide Association Study Pipeline for Enrichment
Platforms.” *Bioinformatics*.
<http://dx.doi.org/10.1093/bioinformatics/bty069>.

Turner, Stephen. 2017. *qqman: Q-Q and Manhattan Plots for Gwas Data*.
<https://CRAN.R-project.org/package=qqman>.

Watson, Hunna J., Zeynep Yilmaz, Laura M. Thornton, Christopher Hübel,
Jonathan R. I. Coleman, Héléna A. Gaspar, Julien Bryois, et al. 2019.
“Genome-Wide Association Study Identifies Eight Risk Loci and Implicates
Metabo-Psychiatric Origins for Anorexia Nervosa.” *Nature Genetics*,
July, 1. <https://doi.org/10.1038/s41588-019-0439-2>.

Wickham, Hadley. 2016. *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

Yang, Jian, S. Hong Lee, Michael E. Goddard, and Peter M. Visscher.
2011. “GCTA: A tool for genome-wide complex trait analysis.” *American
Journal of Human Genetics* 88 (1): 76–82.
<https://doi.org/10.1016/j.ajhg.2010.11.011>.

Yin, Tengfei, Dianne Cook, and Michael Lawrence. 2012. “ggbio: An R
Package for Extending the Grammar of Graphics for Genomic Data.” *Genome
Biology* 13 (8): R77.

[1] Because the majority of the p-values are larger than 0.05, using
`y.thresh` can be helpful to decrease rendering time. However, the
p-value range between 0.05 and 1 can be interesting for model
validation.
