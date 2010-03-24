png("stacked_stats.png", width=640, height=480)

jjplot( ~ line(lty="dashed", col = "red") : hist() + 
              bar(width = 0.1) : hist() : jitter(xfactor = 1) + 
              Sepal.Length, data = iris)

dev.off()

jjplot(Sepal.Length ~ abline() : group(fit(), by = Species) +
                      point() + Petal.Length,
       data = iris, color = Species)

png("stacked_geoms.png", width=640, height=480)

jjplot(Sepal.Length ~ (point(col = "blue", size=3) +
                       line(col = "red", lty="dashed") +
                       bar(width=0.25)) : hist() +
       Petal.Length, data = iris)

dev.off()

### EXAMPLES THAT WORK ###
df <- data.frame(name = factor(letters[1:6]),
                 value = rnorm(6 * 6),
                 foo = rnorm(6 * 6),
                 type = rep(factor(month.name[1:6]), each = 6))


png('density.png', width=480, height=900)

quartz()

source("jjplot.R")



data <- read.csv("~/Downloads/tmp_hipal_jonchang_104098.csv")

data <- rbind(data,
              transform(data[data$count_1 == 2,], count_1 = 1),
              transform(data[data$count_1 == 3,], count_1 = 1))

jjplot(~ vline(as.POSIXct("2010-01-12 21:53:09 UTC"), lty="dashed") +
       area() : density() +
       as.POSIXct(time, origin='1970-01-01'),
       data = data)

jjplot(sqrt(foo^2) ~ point() + value,       
       data = df, color = type, 
       facet.x = name, facet.nrow = 2)

jjplot(~ area() : group(density(), by = type) +
       area() : density() + value,
       data = df, fill = type, alpha = 0.5,
       facet.x = type, facet.nrow = 3)

jjplot(~ area() : group(density(), by = Species) +
       area() : density() + Sepal.Length,
       data = iris, fill = Species, alpha = 0.5,
       facet.y = Species, facet.nrow = 3)

## jjplot(~ area() : group(group(density(), by = day), by = sex) +
jjplot(~ area() : group(density(), by = day:sex) +       
       area() : group(density(), by = sex) +
       I(tip / total_bill),
       data = tips, fill = day, alpha = 0.5,
       facet.y = day, facet.x = sex,
       ylab = "")

jjplot(tip ~ abline() : group(group(fit(), by = day), by = sex) +
       abline(lty = "dashed") : fit() + 
       point() + total_bill,
       data = tips, color = day, alpha = 0.5,
       facet.y = day, facet.x = sex)

dev.off()

source("jjplot.R")

jjplot(~ bar(width = .1) : group(hist(), by = sex) +
       bar(width = .1) : hist() +
       tip,
       data = tips, facet.x = sex, fill = sex, alpha = 0.1)


png("sorted_stats%03d.png", width=480, height=480)

jjplot(name ~ point() + value,
       data = df, color = type, facet.y = type, facet.nrow = 2)

jjplot(name ~ point() : sort(y = value) + value,
       data = df, color = type, facet.y = type, facet.nrow = 2)

source("jjplot.R")



jjplot(value ~ point() : group(sort(x = value), by=type) + name,
       data = df, color = type, facet.y = type)

dev.off()

png("faceted_stats.png", width=480, height=300)

df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)
jjplot(Murder ~ abline(lty = "dashed") : fit() +
       abline() : group(fit(), by = region) +
       point() + Income,
       data = df, color = region, facet.x = region,
       facet.nrow = 2)

dev.off()

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

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)
jjplot(f ~ point(alpha = 0.1) : jitter(yfactor=1, xfactor=1) + x,
       data = df, color = f)

