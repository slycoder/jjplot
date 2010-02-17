source("jjplot.R")

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

qplot.fast(x, f, data = df,
           alpha = 0.05, color = f,
           qplot.jitter(yfactor = 1, xfactor=1),
           qplot.point())

qplot.fast(x + 2, y, data = df,
           alpha = 0.10, color = f,
           qplot.point(),
           qplot.facet(qplot.fit(), facet = f),
           qplot.abline(),
           qplot.fun.y(mean),
           qplot.hline(lty = "dashed"))
