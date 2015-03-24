# Introduction #

jjplot is a plotting library for R inspired by ggplot2, written using grid.  It intends to make it easy to do exploratory data analysis through a rich repertoire of expressive and fast plots.

jjplot is **NOT** a replacement for ggplot2, since the latter is still _way_ more functional than jjplot.  But in many simple cases, we think that jjplot will suffice.

# Installation #
Simply download the source from the "Source" tab above.  In R, all you need to do is `source("jjplot.R")` and you're ready to go.  See jjplot.test.R (or documentation below) for example usages.

# What's new #

### 2010/25/02 ###
  * Make subplots aesthetically pleasing.
  * Package as library.

### 2010/19/02 ###
  * Rewrote in grid.

### 2010/18/02 ###
  * Added text, line, box and bar geoms.
  * Example:
```
df <- data.frame(x = 1:50,
                 y = rnorm(50))

jjplot(x, y, data = df,
       xlab = "song number",
       ylab = "bpm",
       jjplot.line(),
       jjplot.point(),
       jjplot.fun.y(mean),
       jjplot.hline(lty = "dashed", col = "red"),
       jjplot.text(label = "Some label",
                   x = 50, y = -2,
                   hjust = 1, vjust = 0))
```
![http://jjplot.googlecode.com/files/jjplot_test_005.png](http://jjplot.googlecode.com/files/jjplot_test_005.png)

```
jjplot(x, y, data = df,
       xlab = "song number",
       ylab = "bpm",
       fill = y,
       jjplot.bar(col = "black"))
```
![http://jjplot.googlecode.com/files/jjplot_test_006.png](http://jjplot.googlecode.com/files/jjplot_test_006.png)

```
df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)

jjplot(region, Income, data = df,
       fill = region,
       jjplot.group(jjplot.quantile(),
                             by = region),
       jjplot.box())
```
![http://jjplot.googlecode.com/files/jjplot_test_007.png](http://jjplot.googlecode.com/files/jjplot_test_007.png)

### 2010/17/02 ###
  * Numerous bug fixes with color/fill aesthetic assignments.
  * Parameters to set titles/labels.
  * Added `grid` parameters to plot facets in subplots.
  * Example:
```

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
```

![http://jjplot.googlecode.com/files/jjplot_test_004.png](http://jjplot.googlecode.com/files/jjplot_test_004.png)

  * Fills and numeric color scales
  * Better handling of axis labels.
  * Example:
```
df <- data.frame(state = rownames(state.x77),
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))

jjplot(state, variable, data = melt(df),
       alpha = 1.0, fill = value,
       jjplot.point(pch = 22, size = 2))
```
![http://jjplot.googlecode.com/files/jjplot_test_003.png](http://jjplot.googlecode.com/files/jjplot_test_003.png)

# Todo #
I will add functionality as I need it.  Here is the short list:

  * Legends
  * manual color scales
  * scale\_shape
  * Faceting along both axes

# Concepts #
The workhorse function of jjplot is `jjplot`, which takes the following formals:

```
  jjplot(x = NULL, y = NULL, data = NULL, alpha = NULL, color = NULL, facet.y = NULL, facet.x = NULL, ...)
```

  * `x` - An expression to map to the x-axis.
  * `y` - An expression to map to the y-axis.
  * `data` - A data frame in whose context expressions will be evaluated.
  * `alpha` - An expression to map to the alpha channel.
  * `color` - An expression to map to the colors of each element in the data frame.  This expression may be a factor or a numeric.   If it is a factor, colors will be chosen automatically by choosing maximally distinct hues for each factor.  For numerics, a continuous scale will be chosen.
  * `fill` - An expression to map to the colors of each element in the data frame, like `color`.  Unlike `color`, fill will be used to specify the background or fill of visual elements.
  * `facet.x` and `facet.y` - Build subplots for each facet defined by the factor passed to these functions (see example above for details).  Note that these implicitly wrap each subsequent statistic in `jjplot.group` (see below).
  * `...` - Statistic and geometry functions to apply to the data.

After the mapping performed by named arguments, each statistic and geometry function is successively applied.  Statistic functions can be understood as transformations of the data: they take a data frame, compute statistics over that data, and produce a new data frame.  Geometry functions take the data frames output by statistic functions and display them graphically.


## Statistics ##
  * `jjplot.identity` - Returns the original data unchanged.  **This statistic is implicitly called as the first statistic**.
  * `jjplot.fun.x(f)` - Computes a function `f` over the x margin of the data.  Outputs column `x` with the computed statistic.
  * `jjplot.fun.y(f)` - Computes a function `f` over the y margin of the data.  Outputs column `y` with the computed statistic.
  * `jjplot.fit()` - Produces a least-squares fit via `lm` over `y ~ x`.   Outputs columns  `a` and `b` for the slope and intercept of the fit.
  * `jjplot.jitter(xfactor = 0, yfactor = 0)` - Returns the original data jittered by the amounts given as parameters.
  * `jjplot.quantile()` - For each `x`, computes the 0th, 25th, 50th, 75th, and 100th quantiles over `y` values.
  * `jjplot.table()` - Computes the frequency of each `x`
  * `jjplot.ccdf()` - Computes the complementary cumulative density of `x`
  * `jjplot.group(f, by)` - Calls the statistic function `f` on each subset of the data defined by factor `by`.  Outputs the columns of `f` along with a column indicating the associated factor of each data point.

## Geometries ##
Note that many of these typical graphical parameters such as `lwd` and `lty`.
  * `jjplot.hline()` - Draw horizontal lines at positions given by the column `y`.
  * `jjplot.vline()` - Draw vertical lines at positions given by the column `x`.
  * `jjplot.abline()` - Draw lines with slope and intercept given by the columns `a` and `b`.
  * `jjplot.point()` - Draw points at positions given by `x` and `y` (i.e., a scatterplot).
  * `jjplot.line()` - Draw lines between `x` and `y` points (i.e., a lineplot).
  * `jjplot.text()` - Draw text at `x`, `y` locations using `label`.
  * `jjplot.bar()` - Draw bars centered at each `x` of height `y`.
  * `jjplot.box()` - Draw a boxplot.

# Examples #
```
df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)
```

```
> jjplot(x, f, data = df,
         alpha = 0.10, color = f,
         jjplot.jitter(yfactor = 1, xfactor=1),
         jjplot.point()))
```

This function will first set x values to the numeric column `x` in the data frame.
The y values will be set to the factor `f`.   Color will also be associated with `f`.
The points are jittered along both the x and y axis, and then these jittered points are plotted as a scatter plot.   This visualization is a richer version of a boxplot.

![http://jjplot.googlecode.com/files/jjplot_test_001.png](http://jjplot.googlecode.com/files/jjplot_test_001.png)

```
> jjplot(x + 2, y, data = df,
         alpha = 0.10, color = f,
         jjplot.point(),
         jjplot.group(jjplot.fit(), by = f),
         jjplot.abline(),
         jjplot.fun.y(mean),
         jjplot.hline(lty = "dashed")))
```

This function will set the x values to `x + 2` and the y values to the `y` column in the data frame.  Once again, points will be colored according to `f`.   The first `jjplot.point()` will then plot these as a scatter plot (because of the implicit identity statistic).  The next statement applies a least-squares fit to each subset of the data defined by `f`, and the following line draws lines corresponding to these fits.   Finally, the last two lines compute the mean y value of all the points in the data frame and draws a dashed horizontal line at the mean value.

![http://jjplot.googlecode.com/files/jjplot_test_002.png](http://jjplot.googlecode.com/files/jjplot_test_002.png)

# Performance Comparison #
**Warning, these may be out of date!**
## jjplot ##
```
> system.time(jjplot(x, f, data = df,
                     alpha = 0.10, color = f,
                     jjplot.jitter(yfactor = 1, xfactor=1),
                     jjplot.point()))
utilisateur     système      écoulé 
      0.249       0.006       0.267 

> system.time(jjplot(x + 2, y, data = df,
                     alpha = 0.10, color = f,
                     jjplot.point(),
                     jjplot.group(jjplot.fit(), by = f),
                     jjplot.abline(),
                     jjplot.fun.y(mean),
                     jjplot.hline(lty = "dashed")))
utilisateur     système      écoulé 
      0.355       0.011       0.388 
```

## ggplot2 ##
```
> system.time(print(qplot(x, f, data = df,
                          alpha = I(0.1), colour =f,
                          geom = "jitter")))
utilisateur     système      écoulé 
      1.629       0.023       1.669 

> system.time(print(qplot(x + 2, y, data = df,
                          alpha = I(0.1), colour = f) +
                    geom_smooth(method = "lm") +
                    geom_hline(aes(yintercept = mean(y)))))
utilisateur     système      écoulé 
      2.659       0.045       2.718 
```