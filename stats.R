### STATS ###
stats.list <- c("fun.x",
                "fun.y",
                "fit",
                "jitter",
                "identity",
                "quantile",
                "table",
                "ccdf",
                "cumsum",
                "hist",
                "group")

is.stat <- function(layer.call) {
  return(is.call(layer.call) && as.character(layer.call[[1]]) %in% stats.list)
}

jjplot.table <- function(data, x.expr, y.expr, log.y = FALSE) {
  ## FIXME: log.y?
  tt <- table(data$x)
  df <- data.frame(x = names(tt), y = as.numeric(tt))
  if (log.y) {
    df$y <- log10(df$y)
  }
  list(data=df, x.expr=substitute(Count(x), list(x=x.expr)), y.expr=y.expr)
}

jjplot.hist <- function(data, x.expr, y.expr,
                        align = c("left", "right", "middle"),
                        breaks = 20,
                        density = TRUE,
                        log.y = FALSE) {
  align <- match.arg(align)

  h <- hist(data$x, breaks = breaks, plot = FALSE)
  if (align == "left") {
    hx <- h$breaks[-length(h$breaks)]
  } else if (align == "right") {
    hx <- h$breaks[-1]
  } else {
    hx <- h$mids
  }
  
  cs <- h$density
  if (!density) {
    cs <- cs * length(facet.data$x)
  }
  ## FIXME: log.y
  if (log.y) {
    dens <- log10(cs)
  } else {
    dens <- cs
  }
  
  list(data = data.frame(x = hx, y = dens),
       x.expr = x.expr,
       y.expr = substitute(Count(x), list(x = x.expr)))
}

jjplot.jitter <- function(data, x.expr, y.expr,
                          xfactor = 0, yfactor = 0) {
  list(data = transform(data,
         x = jitter(as.numeric(x), xfactor),
         y = jitter(as.numeric(y), yfactor)),
       x.expr = if (xfactor != 0) substitute(jitter(x), list(x=x.expr)) else x.expr,
       y.expr = if (yfactor != 0) substitute(jitter(x), list(x=y.expr)) else y.expr)
}
    
jjplot.fit <- function(data, x.expr, y.expr) {
  model <- lm(data$y ~ data$x)
  list(data.frame(b = coef(model)[1],
                  a = coef(model)[2]),
       x.expr = x.expr, y.expr = y.expr)
}

jjplot.fun.x <- function(data, x.expr, y.expr, fun) {
  list(data = data.frame(x = fun(data$x)),
       x.expr = x.expr, y.expr = y.expr)
}

jjplot.fun.y <- function(data, x.expr, y.expr, fun) {
  list(data = data.frame(y = fun(data$y)),
       x.expr = x.expr, y.expr = y.expr)
}

