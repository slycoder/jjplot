
quartz()

##### Examples ######

df <- data.frame(x = 1:50,
                 y = rnorm(50))

png("jjplot_test_006.png", width=640, height=480)

qplot.fast(x, y, data = df,
           xlab = "song number",
           ylab = "bpm",
           fill = y,
           qplot.bar(col = "black"))

dev.off()

png("jjplot_test_005.png", width=640, height=480)

qplot.fast(x, y, data = df,
           xlab = "song number",
           ylab = "bpm",
           qplot.line(),
           qplot.point(),
           qplot.fun.y(mean),
           qplot.hline(lty = "dashed", col = "red"))

dev.off()

png("jjplot_test_004.png", width=640, height=480)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)

qplot.fast(Income, Murder, data = df,
           color = region,
           grid.y = region,
           qplot.identity(),
           qplot.point(),
           qplot.fit(),
           qplot.abline())

dev.off()

png("jjplot_test_007.png", width=640, height=480)

qplot.fast(region, Income, data = df,
           fill = region,
           qplot.facet(qplot.quantile(),
                       facet = region),
           qplot.box())

dev.off()

png("jjplot_test_003.png", width=640, height=480)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))

require("ggplot2")
melted <- melt(df, id.vars = c("state", "region"))

qplot.fast(variable, state, data = melted,
           alpha = 1.0, fill = value,
           ylab = "", xlab = "",
           qplot.point(pch = 22, size = 2))

dev.off()

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

png("jjplot_test_%03d.png", width=640, height=480)

source("jjplot.R")
system.time(qplot.fast(x, f, data = df,
                       alpha = 0.10, color = f,
                       qplot.jitter(yfactor = 1, xfactor=1),
                       qplot.point()))
            
source("jjplot.R")
system.time(qplot.fast(x + 2, y, data = df,
                       alpha = 0.10, color = f,
                       qplot.point(),
                       qplot.facet(qplot.fit(), facet = f),
                       qplot.abline(),
                       qplot.fun.y(mean),
                       qplot.hline(lty = "dashed")))

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

      
