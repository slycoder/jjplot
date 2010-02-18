require("ggplot2")
source("jjplot.R")

df <- data.frame(state = rownames(state.x77),
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))

quartz()

png("jjplot_test_003.png", width=640, height=480)

qplot.fast(state, variable, data = melt(df),
           alpha = 1.0, fill = value,
           qplot.point(pch = 22, size = 2))

dev.off()

##### Examples ######

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

png("jjplot_test_%03d.png", width=640, height=480)

system.time(qplot.fast(x, f, data = df,
                       alpha = 0.10, color = f,
                       qplot.jitter(yfactor = 1, xfactor=1),
                       qplot.point()))
            
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

      
