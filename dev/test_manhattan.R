
library(GWAS.utils)
data("giant")
?giant
theme_set(theme_gwas())

## default: for -log10(P), by default chr is numeric
qp <- ggplot(giant) +
  stat_manhattan(aes(pos = POS, y = -log10(P), chr = CHR)) +
  geom_hline(yintercept = 8) +
  ggtitle("GIANT summary statistics (by default CHR is numeric)")
print(qp)

## add nice color palette
pal <- wesanderson::wes_palette("Zissou1", 22, type = "continuous")
qp + scale_color_gradientn(colours = pal)

## chr factor
qp <- ggplot(giant) +
  stat_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), chr.class = "character") +
  geom_hline(yintercept = 8) +
  ggtitle("GIANT summary statistics (CHR is now a character/factor)")
print(qp)
## adding a nice color palette
qp + scale_color_manual(values = wesanderson::wes_palette("Zissou1", 22, type = "continuous"))

## turn all points black
qp <- ggplot(giant) +
  stat_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), color = "black", alpha = I(0.4)) +
  geom_hline(yintercept = 8) +
  ggtitle("GIANT summary statistics")
print(qp)

## set lower threshold
qp <- ggplot(data = giant) +
  stat_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), y.thresh= c(2, NA)) +
  geom_hline(yintercept = 8) +
  ggtitle("GIANT summary statistics")
print(qp)


## for effect sizes
qp <- ggplot(data = giant) +
  stat_manhattan(aes(pos = POS, y = BETA, chr = CHR)) +
  ggtitle("GIANT effect sizes")
print(qp)

## use rastr
qp <- ggplot(data = giant) +
  stat_manhattan(aes(pos = POS, y = -log10(P), chr = CHR), geom = ggrastr:::GeomPointRast) +
  geom_hline(yintercept = 8) +
  ggtitle("GIANT summary statistics (rastr)")
print(qp)

## facetting

giant <- giant %>% dplyr::mutate(gr = dplyr::case_when(BETA <= 0 ~ "Neg effect size", BETA > 0 ~ "Pos effect size"))## generate two groups

qp <- ggplot(data = giant) +
  stat_manhattan(aes(pos = POS, y = BETA, chr = CHR)) +
  ggtitle("GIANT summary statistics") +
  facet_wrap(~gr)
print(qp)




## for faceting: doing this per facet or doing this overall and then facetting?

## how it should look like
calc.cumsum <- function(data) data %>%
    dplyr::arrange(CHR, BP) %>%
    mutate(p.log = -log10(P)) %>%
    filter(p.log >= 2) %>%
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
