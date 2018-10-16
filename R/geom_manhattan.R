
##make manhattan plot
## ggplot(data, aes(x = c(chr, pos), y = P)) 
## > should turn this into

## some copied from here: 
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r


## real distance
## dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))

## new x axis
## med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
## ------------------
StatManhattan <- ggproto("StatManhattan", Stat,
                         compute_group = function(data, scales, params, y.thresh) {
                           
                           ## y.thesh: vector
                           ## if c(NA, something), then y <= something
                           ## if c(something, NA), then y <= something
                           ## only include points that are above this threshold
                           if(!is.null(y.thresh))
                           {
                             if(!is.na(y.thresh[1]))
                             {
                               data <- data %>% filter(y >= y.thresh[1])
                             }
                             if(!is.na(y.thresh[2]))
                             {
                               data <- data %>% filter(y <= y.thresh[2])
                             }
                           }
                           

                           ## equidistance
                           data2 <- data %>% dplyr::arrange(chr, pos) %>% dplyr::mutate(tmp = 1, cumsum.tmp = cumsum(tmp))
                           ## real distance
                           # dat <- gwasResults %>% arrange(CHR, BP) %>% mutate(tmp = diff from start, x = cumsum(tmp))
                           
                           
                           ## new x axis
                           #                       med.dat <- data %>% group_by(group) %>% summarise(median.x = median(cumsum.tmp))
                           #scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
                           
                           
                           data.frame(x = data2$cumsum.tmp, y = data2$y, colour = as.character(data2$chr))
                           
                         },
                         
                         required_aes = c("y", "pos", "chr"),
                         default_aes = aes(y = stat(y), x = stat(x), colour = stat(colour))
                         
)

stat_manhattan <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, y.thresh = NULL, ...) { #, dparams = list()
  layer(
    stat = StatManhattan, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, 
                  y.thresh = y.thresh,
                  ...)
  )
}

geom_manhattan <- stat_manhattan

## Example
## ------------
dat <- qqman::gwasResults# %>% filter(P < 0.05)#, chr="CHR", bp="BP", snp="SNP", p="P" )

## default: for -log10(P)
qp <- ggplot(dat %>% mutate(CHR2 = as.character(CHR))) + 
  stat_manhattan(aes(pos = BP, y = -log10(P), chr = CHR)) + 
  geom_hline(yintercept = 8) + 
  ggtitle("sfsdfsdf")
print(qp)

## for P values
qp <- ggplot(dat %>% mutate(CHR2 = as.character(CHR))) + 
  stat_manhattan(aes(chr = CHR, pos = BP, y = P), y.thresh = c(1e-8, 0.05)) + 
  geom_hline(yintercept = 0.05) + 
  ggtitle("sfsdfsdf")
print(qp)


## for faceting: doing this per facet or doing this overall and then facetting? 

## how it should look like
calc.cumsum <- function(data) data %>% dplyr::arrange(CHR, BP) %>% 
  mutate(p.log = -log10(P)) %>% filter(p.log >= 2) %>%
  dplyr::mutate(temporary = 1) %>% 
  dplyr::mutate(cumsum.tmp = cumsum(temporary))
  
ggplot(calc.cumsum(dat)) + 
  geom_point(aes(x = cumsum.tmp, y = p.log)) + 
  geom_hline(yintercept = 8)

datgg <- ggplot_build(qp)













# 
# 
# ## why characters
# ## why not working when "chrpos"
# ## how to add x1 and x2
# 
# data <- qqman::gwasResults %>% filter(P < 0.05) %>% mutate(x = glue.xaxis(CHR, BP)) %>% separate(x, c("x1", "x2"), ":", convert = TRUE)
# 
# ## equidistance
# data <- data %>% 
#   dplyr::arrange(x1, x2) %>% 
#   dplyr::mutate(temporary = 1) %>% dplyr::mutate(cumsum.tmp = cumsum(temporary))
# str(data)
# 
# 
# 
# 
# 
# 
# 
# 
# ## default + color
# qp <- ggplot(dat, aes(x = x, y = -log10(P), colour = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
# 
# ## different x axis scheme
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), colour = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8) + 
#   scale_x_continuous(breaks = med.dat$median.x, labels = med.dat$CHR)
# 
# print(qp)
# 
# 
# ## adding nice stuff , like color 
# dat <- dat %>% mutate(CHR.bin = CHR %% 2, CHR.col = case_when(
#   CHR.bin == 0 ~ "black",
#   CHR.bin == 1 ~ "red"
# )) ## bin CHR to have alternating colors
# ## mutate(CHR = case_when(CHR == "X" ~ 23, CHR == "Y" ~ 24, CHR == "MT" ~ 24, TRUE ~ CHR))  ## if X, Y, MT
# 
# ## like alternating color 
# qp <- ggplot(data = NULL) + 
#   stat_manhattan(data = dat, aes(x = cumsum.tmp, y = -log10(P), colour = CHR.col, group = CHR.col)) + 
#   geom_hline(yintercept = 8)
# print(qp) 
# 
# ## group
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P), group = factor(CHR))) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
# 
# ## facet
# qp <- ggplot(dat, aes(x = cumsum.tmp, y = -log10(P))) +  
#               facet_wrap(~ CHR) + 
#   stat_manhattan() + 
#   geom_hline(yintercept = 8)
# print(qp)
# 
