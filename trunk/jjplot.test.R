png("stacked_stats.png", width=640, height=480)

date.data <-
  data.frame(t = as.Date(sample(10000, 10), origin = "1970-01-01"),
             v = rnorm(10))

source("jjplot.R")
jjplot(v ~ point() + t, date.data,
       theme = jjplot.theme("bw"))


quartz()

require(reshape)

source("jjplot.R")
jjplot(tip ~ box() : group(quantile(), by = sex) : color(day)  + day,
       data = tips,
       facet.x = sex)

jjplot(day ~ point() : log(x) + tip,
       facet.y = sex,
       data = subset(tips, tip > 4.5),
       squash.unused = T)

jjplot(day ~ tile(lwd = 3, lty = "dashed") :
       color(tip) + sex,
       data = tips)



jjplot( ~ line(lty="dashed", col = "red") : hist() + 
              bar(width = 0.1) : hist() : jitter(xfactor = 1) + 
              Sepal.Length, data = iris)

jjplot( ~ bar(width = 0.005) : hist() : log(x) +
       Sepal.Length, data = iris,
       theme = jjplot.theme("bw"),
       log.x = T)


dev.off()

jjplot(Sepal.Length ~ (abline() : group(fit(), by = Species) +
                       point()) : color(Species) + Petal.Length,
       data = iris)

jjplot(Sepal.Length ~ point() : shape(Species) + Petal.Length,
       data = iris, theme=jjplot.theme("bw"))

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

## Extra stats
jjplot.stat.kmeans <- function(state, K, use.y = FALSE) {
  if (use.y) {
    km <- kmeans(cbind(state$data$x, state$data$y), K)
  } else {
    km <- kmeans(state$data$x, K)    
  }
  state$data$cluster <- factor(km$cluster)
  state
}

jjplot(Petal.Length ~ point(alpha=1) : 
       color(cluster):kmeans(5, use.y = T) : shape(Species) : size(Petal.Width) + Sepal.Length,
       data = iris)


source("jjplot.R")
jjplot(tip ~ point() : color(tip)  + day, data = tips)


jjplot(tip ~ (abline() : group(fit(), by = day: sex) +
point(alpha = 0.5)) : color(day) +
abline(lty = "dashed") : color(a): fit() + total_bill,
data = tips,
facet.y = day, facet.x = sex,
theme = jjplot.theme.top.strip(jjplot.theme("bw"),
                               c("Female", "Male")))

jjplot(~ area() : group(density(), by = day:sex) : color(day, alpha = 0.5) + 
       area() : group(density(), by = day) +
       I(tip / total_bill),
       data = tips, 
       facet.y = day, facet.x = sex,
       xlab = "tip fraction",
       ylab = "")


jjplot( ~ area() : group(density(), cluster) :
          color(cluster, alpha = 0.5) : kmeans(3) +
          total_bill,
       data = tips)

jjplot(tip ~ (point() +
       abline() : group(fit(), cluster)) : color(cluster) : kmeans(3) +
       total_bill,
       data = tips)


jjplot( ~ bar(width = 0.25) : table() + I(round((tip * 100) %% 10)),
       data = tips)

jjplot(tip ~ (abline() : color(a) : group(fit(), by = day: sex) +
point(alpha = 0.5)) +
abline(lty = "dashed") : fit() + total_bill,
data = tips,
facet.y = day, facet.x = sex)

# this is broken. not really sure what the correct behavior ought to be though.
jjplot( ~ line(lty="dashed", ordered=F) : color(y) : hist() + 
bar(width = 0.1) :color(y):hist() : jitter(xfactor = 1) + 
Sepal.Length, data = iris)


jjplot(tip ~ (abline() : group(fit(), by = day: sex) +
              point(alpha = 0.5)) : color(day) +
       abline(lty = "dashed") : fit() + total_bill,
       data = tips,
       facet.y = day, facet.x = sex)

jjplot( ~ bar(width = 0.1) : color(y) : hist() : jitter(xfactor = 1) + 
       Sepal.Length, data = iris)



jjplot(tip ~ (abline() : group(group(fit(), by = day), by = sex) +
       abline(lty = "dashed") : fit() + 
       point()) : color(day, alpha = 0.5) + total_bill,
       data = tips,
       facet.y = day, facet.x = sex)

dev.off()



jjplot(~ bar(width = .1) : group(hist(), by = sex) : color(sex, alpha = 0.75) +
       bar(width = .1) : hist() +
       tip,
       data = tips, facet.x = sex)


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
       abline() : group(fit(), by = region) : color(region) +
       point() + Income,
       data = df, facet.x = region,
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
           abline() : group(fit(), by = f) : color(f) +
           point(alpha = 0.1) : jitter(xfactor = 1) +
           x, data = df)

jjplot(~ (point(alpha=0.1) : jitter(xfactor=1) + box() : group(quantile(), by=f)) : color(f) + f, data = df)

# Test facets where one can reorder the facets
df <- data.frame(t=rnorm(20),
                 z=rnorm(20)^2,
                 ylabel=rep(c("ya","yb"),each=10),
                 xlabel=rep(rep(c("xa","xb"),each=5),2))
jjplot(z ~ point() + t, data=df,
       facet.x=factor(xlabel),facet.y=factor(ylabel),
       facet.yorder="reverse")

# Test if labels for Date objects shown as dates
df <- data.frame(y=rnorm(20),
                 label=rep(c("a","b"),each=10),
                 x=as.Date(seq(15000,16000,length.out=20),
                   origin="1970/01/01"))

jjplot(y ~ point():color(label) + x,
       data=df, facet.y=factor(label), theme=jjplot.theme("bw"))



# Test simple plot.
x <- seq(0,1,by=.1)+1
f <- function(x) x^2
y <- f(x)
jjplot(y ~ point() + x)

### EXAMPLES THAT DON'T WORK ###

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)
jjplot(f ~ point(alpha = 0.1) : jitter(yfactor=1, xfactor=1) + x,
       data = df, color = f)

