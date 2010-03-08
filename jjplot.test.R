quartz()

data <- data.frame(x = rnorm(100), y = rnorm(100),
                   f = factor(c('A', 'B', 'C', 'D')))

source("jjplot.R")
jjplot(y ~ hline() : fun.y(mean) +
           abline() : fit() +
           point(alpha = 0.2) : jitter(xfactor = 1) +
           x, data = data, color = f)

df <- data.frame(x = sample(factor(LETTERS[1:10]), 100, replace=TRUE))

jjplot(x, data = df,
       ylab = "count",
       jjplot.table(),
       jjplot.bar(width = 0.5))

##### Examples ######

df <- data.frame(x = 1:50,
                 y = rnorm(50))

png("jjplot_test_006.png", width=640, height=480)

jjplot(x, y, data = df,
       xlab = "song number",
       ylab = "bpm",
       fill = y,
       jjplot.bar(col = "black"))

dev.off()

png("jjplot_test_005.png", width=640, height=480)

jjplot(x, y, data = df,
       xlab = "song number",
       ylab = "bpm",
       jjplot.line(),
       jjplot.point(),
       jjplot.fun.y(mean),
       jjplot.hline(lty = "dashed", col = "red"))

dev.off()

png("jjplot_test_004.png", width=640, height=480)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)

source("jjplot.R")
jjplot(Income, Murder, data = df,
       color = region,
       grid.y = region,
       jjplot.identity(),
       jjplot.point(),
       jjplot.fit(),
       jjplot.abline())

dev.off()

png("jjplot_test_007.png", width=640, height=480)

jjplot(region, Income, data = df,
       fill = region,
       jjplot.facet(jjplot.quantile(),
                    facet = region),
       jjplot.box())

dev.off()

png("jjplot_test_003.png", width=640, height=480)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))

require("ggplot2")
melted <- melt(df, id.vars = c("state", "region"))

jjplot(variable, state, data = melted,
       alpha = 1.0, fill = value,
       ylab = "", xlab = "",
       jjplot.point(pch = 22, size = 2))

dev.off()

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

png("jjplot_test_%03d.png", width=640, height=480)

system.time(jjplot(x, f, data = df,
                   alpha = 0.10, color = f,
                   jjplot.jitter(yfactor = 1, xfactor=1),
                   jjplot.point()))

system.time(jjplot(x + 2, y, data = df,
                   alpha = 0.10, color = f,
                   jjplot.point(),
                   jjplot.facet(jjplot.fit(), facet = f),
                   jjplot.abline(),
                   jjplot.fun.y(mean),
                   jjplot.hline(lty = "dashed")))

dev.off()

png("ggplot_test_%03d.png", width=640, height=480)

system.time(print(qplot(x, f, data = df,
                        alpha = I(0.1), colour =f,
                        geom = "jitter")))

system.time(print(qplot(x + 2, y, data = df,
                        alpha = I(0.1), colour = f) +
                  geom_smooth(method = "lm") +
                  geom_hline(aes(yintercept = mean(y)))))

dev.off()

      
