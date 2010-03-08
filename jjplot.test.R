quartz()

data <- data.frame(x = rnorm(100), y = rnorm(100),
                   f = factor(c('A', 'B', 'C', 'D')))

source("jjplot.R")
jjplot(y ~ hline(lty = "dashed") : fun.y(mean) +
           abline() : fit() | f +
           point(alpha = 0.2) : jitter(xfactor = 1) +
           x, data = data, color = f)

df <- data.frame(x = sample(factor(LETTERS[1:10]), 100, replace=TRUE))
jjplot( ~ bar(width = 0.5) : table() + x, data = df)

df <- data.frame(x = 1:50, y = rnorm(50))
jjplot(y ~ bar(col = "black") + x, data = df, fill = y)

jjplot(y ~ hline(lty = "dashed", col = "red") : fun.y(mean) +
           point() + line() + x, data = df)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)

jjplot(Income, Murder, data = df,
       color = region,
       grid.y = region,
       jjplot.identity(),
       jjplot.point(),
       jjplot.fit(),
       jjplot.abline())
dev.off()

jjplot(Income ~ box() : quantile() | region + region,
       data = df, fill = region)

require("reshape")
df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))
melted <- melt(df, id.vars = c("state", "region"))

jjplot(state ~ point(pch = 22, size = 2) + variable, data = melted,
       alpha = 1.0, fill = value,
       ylab = "", xlab = "")

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

jjplot(f ~ point() : jitter(yfactor=1, xfactor=1) + x,
       data = data, alpha = 1.0, color = f)
