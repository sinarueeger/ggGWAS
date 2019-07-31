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
---

# Summary

Genome-wide association studies (GWAS) compute associations between each SNP and a trait of interest. The output of a GWAS is a matrix that contains the summary statistics for each SNP per line. For example the SNP identifier, chromosome, position, effect size, standard error, p-value effect allele, and sometimes the minor allele frequency (ref to gcta). 

A set of model validation steps are performed after the GWAS. One such step is to check the p-value distribution for inflation due to a mispecified GWAS model. This is done by calculating the genomic inflation factor (median...), looking at the Q-Q plot of the p-values and displaying the p-values along the chromosomal position (Manhattan plot). 

While the GWAS computation itself is often done with command line tools (ref), the exploration and validation of the results is done in conventional statistical software, such as R (ref). 

Some of the post-GWAS plots will only be looked at once (e.g. a QQplot), but others (e.g. the Manhattan plot) might become the primary figure of an article. 

Therefore, fast plotting is important, but also versatile plotting (being able to add layers and annotation). 

``ggplot2`` offers such a versatile plotting package. However, ``ggplot2`` is known for its low speed (ref). One reason for this is that a ggplot2 object contains the dataset itself plus adaptations of it. 

With ``ggGWAS`` we propose a ``ggplot2`` extension for GWAS QQplots (`geom_qqplot`) and Manhattan plots (`geom_manhattan`). For  `geom_qqplot` there is additionally a hexagon version that reduces computing time by xx.

## Review


# Citations

- heaxongs: freytag

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:

- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

<!----- Figures can be included like this: ![Example figure.](figure.png) ----->

# Acknowledgements

<!-------- We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.-------->

- Flavia Hodel contributed parts of the Manhattan plot
- Fellay lab

# References
