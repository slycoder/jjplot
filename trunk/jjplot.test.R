quartz()

source("jjplot.R")


jjplot(Sepal.Length ~ abline() : group(fit(), by = Species) + point() + Petal.Length,
       data = iris, color = Species)

jjplot(Sepal.Length ~ (point(col = "blue", size=3) +
                       line(col = "red", lty="dashed") +
                       bar(width=0.25)) : hist() : jitter(xfactor = 1) +
       Petal.Length, data = iris)


### EXAMPLES THAT WORK ###
data <- data.frame(x = rnorm(100), y = rnorm(100),
                   f = factor(c('A', 'B', 'C', 'D')))

jjplot(y ~ line(col = "red", lty = "dashed") + bar(width=0.1) : hist() : jitter(xfactor = 1) + x, data = data)

jjplot(y ~ vline(lty = "dashed", col = "red") : fun.x(mean) +
           point() + line() + x, data = data)


df <- data.frame(x = 1:50, y = rnorm(50))
jjplot(y ~ bar(col = "black") + x, data = df, fill = y)

jjplot(y ~ hline(lty = "dashed", col = "red") : fun.y(mean) +
           point() + line() + x, data = df)

require("reshape")
df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))
melted <- melt(df, id.vars = c("state", "region"))

jjplot(state ~ point(pch = 22, size = 2) + variable, data = melted,
       alpha = 1.0, fill = value,
       ylab = "", xlab = "")

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)
jjplot(Income ~ box() : group(quantile(), by = region) + region,
       data = df, fill = region)

df <- data.frame(x = sample(factor(LETTERS[1:10]), 100, replace=TRUE))
jjplot( ~ bar(width = 0.5) : table() + x, data = df)

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

jjplot(y ~ hline(lty = "dashed") : fun.y(mean) +
           abline() : group(fit(), by = f) +
           point(alpha = 0.1) : jitter(xfactor = 1) +
           x, data = df, color = f)

### EXAMPLES THAT DON'T WORK ###
source("jjplot.R")

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)
jjplot(Murder ~ abline() : fit() + point() + Income,
       data = df, color = region, facet.y = region)

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)
jjplot(f ~ point(alpha = 0.1) : jitter(yfactor=1, xfactor=1) + x,
       data = df, color = f)

