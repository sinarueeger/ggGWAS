## - have pretty hexagons (check)
## - reduce size of qqplot object (not really a problem when plotting. independent of size, plotting of hexagons is fast)
## - how does it know to plot hexagons?
## - labels of qqplot







(qp <- ggplot(GWAS.utils::giant, aes(observed = P)) +
  stat_gwas_qq(geom = "line") +
  geom_abline(intercept = 0, slope = 1))

(qp <- ggplot(GWAS.utils::giant, aes(observed = P)) +
    stat_gwas_qq() + stat_gwas_qq(geom = "line") +
    geom_abline(intercept = 0, slope = 1)) +
  xlim(0,2) + ylim(0,2)


d <- ggplot(diamonds, aes(carat, price))
d + geom_hex()


# You can control the size of the bins by specifying the number of
# bins in each direction:
d + geom_hex(bins = 10)
d + geom_hex(bins = 5)

#Depends: R (>= 2.10)


## From here: https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html
## --------------------------------------------------------------------------------------
n.sample <- 1e6
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a", "b"),
   n.sample,
   replace = TRUE
 ))
gg <-ggplot(data = df) + stat_gwas_qq(aes(observed = P)) #+

gg <- ggplot(data = df) + stat_gwas_qq_hex(aes(y = P), fill = "black" , bins = 20) #418x13 table in gb$data


#gg <-ggplot(data = GWAS.utils::giant) + stat_gwas_qq_hex(aes(y = P), colour = "red" ) #418x13 table in gb$data
gg <- ggplot(data = GWAS.utils::giant) + stat_gwas_qq_hex(aes(y = P), fill = "black" , bins = 20) #418x13 table in gb$data

library(pryr)
object_size(gg)

gg <- ggplot(data = GWAS.utils::giant) + stat_gwas_qq_hex(aes(y = P), fill = "black" , bins = 30) #418x13 table in gb$data
#  facet_wrap( ~ CHR, label = label_both) ## leads to a 2'886x13 table
#  geom_abline(intercept = 0, slope = 1)

#ggplot(data = mtcars, aes(displ, hwy, colour = class)) +
#  geom_point() -> gg

#str(gg)
gb <- ggplot_build(gg)
#str(gb)

gt <- ggplot_gtable(gb)
#str(gt)

## compare data
tibble::as_tibble(gb$plot$data) ## original data
tibble::as_tibble(gb$data[[1]]) ## plotting data


library(grid)

grid.newpage()
grid.draw(gt)

