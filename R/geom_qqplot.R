## old code
## ---------

# QQplot <- function(p, neff = NULL, main = "", col = "black", add = FALSE, ...) {
#   p <- p[!is.na(p)]
#   N <- length(p)
#   
#   if (is.null(neff))
#   {
#     p0 <- sort(-log10((1:N)/N-1/(2*N)))
#     col <- ifelse(length(col) > 1, order(-log10((1:N)/N-1/(2*N))), col)
#     
#   }else
#   {
#     p0.tmp <- seq(1/neff, 1, length.out = N)
#     p0 <- sort(-log10(p0.tmp))
#     col <- ifelse(length(col) > 1, order(-log10(p0.tmp)), col)
#   }
#   
#   if(add)
#   {
#     points(p0,sort(-log10(p)),col=col, pch=16, ...)
#   }else{
#     plot(p0,sort(-log10(p)),col=col, pch=16,xlab="Expected -log10(P)",ylab="Observed -log10(P)",
#          main = main, las = 1, ...)
#   }
#   
#   lines(-log10(p0),-log10(p0),type="l",col=gray(0.3))
# }
# 
# QQplot(df$P)
# abline(a=0, b = 1)

## Testing




    
## some copied from here: 
## https://github.com/tidyverse/ggplot2/blob/master/R/stat-qq.r
    
    
## Geom QQ Plot
## ------------------
StatQQplot <- ggproto("StatQQplot", Stat,
                      default_aes = aes(y = stat(observed), x = stat(expected)),
                      
                      required_aes = c("observed"),
                      
                      compute_group = function(data, scales, dparams = list(),
                                               na.rm = FALSE) {
                        
                        observed <- data$observed#[!is.na(data$x)]
                        N <- length(observed)
                       
                        ## expected
                        expected <- sort(-log10((1:N)/N-1/(2*N)))
                        observed <- sort(-log10(observed))
                        data.frame(observed, expected)
                      
                      }
)

stat_qqplot <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatQQplot, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



## Example

n.sample <- 10000
df <- data.frame(P = runif(n.sample), GWAS = sample(c("a","b"), n.sample, replace = TRUE))

## default
qp <- ggplot(df, aes(observed = P)) + 
  stat_qqplot() + 
  geom_abline(intercept = 0, slope = 1) 
print(qp)

## adding nice stuff
qp + 
    theme(aspect.ratio=1) + ## square shaped
    expand_limits(x = -log10(max(df$P)), y = -log10(max(df$P))) + ## identical limits (meaning truely square)
    ggtitle("QQplot") + ## title
  xlab("Expected -log10(P)") + ## axis labels
  ylab("Observed -log10(P)")

## color 
ggplot(df, aes(observed = P, color = GWAS)) + 
    stat_qqplot() + 
    geom_abline(intercept = 0, slope = 1) 

## facet
ggplot(df, aes(observed = P)) + 
  facet_wrap(~GWAS) + 
  stat_qqplot() + 
  geom_abline(intercept = 0, slope = 1) 


## group
ggplot(df, aes(observed = P, group = GWAS)) + 
    stat_qqplot() + 
    geom_abline(intercept = 0, slope = 1)
    