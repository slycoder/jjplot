### STATS ###
.stats.list <- c("fun.x",
                 "fun.y",
                 "fit",
                 "jitter",
                 "identity",
                 "quantile",
                 "table",
                 "ccdf",
                 "cumsum",
                 "hist",
                 "group",
                 "sort",
                 "density")

.is.stat <- function(layer.call) {
  return(is.call(layer.call) && as.character(layer.call[[1]]) %in% .stats.list)
}

.bind.attr.columns <- function(result, data) {
  if (!is.null(data$color)) {
    if (all(data$color == data$color[1])) {
      result$color <- data$color[1]
    } 
  }
  if (!is.null(data$fill)) {
    if (all(data$fill == data$fill[1])) {
      result$fill <- data$fill[1]
    } 
  }
  if (!is.null(data$size)) {
    if (all(data$size == data$size[1])) {
      result$size <- data$size[1]
    } 
  }
  if (!is.null(data$.facet.x)) {
    if (all(data$.facet.x == data$.facet.x[1])) {
      result$.facet.x <- data$.facet.x[1]
    } 
  }
  if (!is.null(data$.facet.y)) {
    if (all(data$.facet.y == data$.facet.y[1])) {
      result$.facet.y <- data$.facet.y[1]
    } 
  }
  result
}

.jjplot.table <- function(data, x.expr, y.expr, log.y = FALSE) {
  ## FIXME: log.y?
  tt <- table(data$x)
  df <- data.frame(x = names(tt), y = as.numeric(tt))
  if (log.y) {
    df$y <- log10(df$y)
  }
  list(data=df, x.expr=x.expr, y.expr = substitute(Count(x), list(x=x.expr)))
}

.jjplot.hist <- function(data, x.expr, y.expr,
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
    cs <- cs * length(data$x)
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

.jjplot.jitter <- function(data, x.expr, y.expr,
                           xfactor = 0, yfactor = 0) {
  data <- transform(data, x = jitter(as.numeric(data$x), xfactor))
  if (!is.null(data$y)) {
    data <- transform(data, y = jitter(as.numeric(data$y), yfactor))    
  }
  list(data = data,
       x.expr = if (xfactor != 0) substitute(jitter(x), list(x=x.expr)) else x.expr,
       y.expr = if (yfactor != 0) substitute(jitter(x), list(x=y.expr)) else y.expr)
}

.jjplot.fit <- function(data, x.expr, y.expr) {
  model <- lm(data$y ~ data$x)
  result <- data.frame(b = coef(model)[1],
                       a = coef(model)[2])
  list(data = .bind.attr.columns(result, data), 
       x.expr = x.expr, y.expr = y.expr)
}

.jjplot.fun.x <- function(data, x.expr, y.expr, fun) {
  list(data = data.frame(x = fun(data$x)),
       x.expr = x.expr, y.expr = y.expr)
}

.jjplot.fun.y <- function(data, x.expr, y.expr, fun) {
  list(data = data.frame(y = fun(data$y)),
       x.expr = x.expr, y.expr = y.expr)
}

.jjplot.quantile <- function(data, x.expr, y.expr) {
  stopifnot(all(data$x == data$x[1]))
  result <- data.frame(data$x[1], t(quantile(data$y)))
  colnames(result) <- c("x", "quantile.0", "quantile.25", "quantile.50", "quantile.75", "quantile.100")
  rownames(result) <- NULL

  list(data = .bind.attr.columns(result, data),
       x.expr = x.expr,
       y.expr = y.expr)
}

.jjplot.ccdf <- function(data, x.expr, y.expr,
                         density = FALSE, maxpoints = FALSE,
                         log.y = FALSE, log.x = FALSE) {
  freqs <- table(data$x)
  df <- data.frame(x=as.numeric(rev(names(freqs))),
                   y=cumsum(rev(freqs)))
  if (density) {
    df$y <- df$y/nrow(df)
  } 
  if (log.y) {
    df$y <- log10(df$y)
  }
  if (log.x && maxpoints != FALSE && is.numeric(maxpoints)) {
    group <- cut(df$x, b=maxpoints)
    df <- do.call(rbind, by(df, group,
                            function(X) X[order(X$x)[floor(length(X)/2)],]))
  }
  
  list(data = df, x.expr = x.expr,
       y.expr = if (density) {
         substitute(Pr(x>=X),list(x=x.expr,X=toupper(x.expr)))
       } else {
         substitute(Count(x>=X),list(x=x.expr,X=toupper(x.expr)))
       })
}

.jjplot.cumsum <- function(data, x.expr, y.expr,
                           decreasing=TRUE,
                           log.y = FALSE) {
  oo <- order(data$x, decreasing=decreasing)
  cs <- cumsum(data$y[oo])
  if (log.y) {
    cs <- log10(cs)
  }      
  list(data = data.frame(x = data$x[oo], y = cs),
       x.expr = x.expr,
       y.expr = substitute(cumsum(x), list(x = y.expr)))
}

.jjplot.group <- function(data, x.expr, y.expr,
                          fun, by) {
  eval.by <- eval(match.call()$by, data)  
  fun.call <- match.call()$fun

  faceted.df <- base:::by(cbind(data, .by = eval.by),
                          eval.by,
                          function(df) {
                            state <- list(data = df,
                                          x.expr = x.expr,
                                          y.expr = y.expr)
                            result <- .call.with.data(fun.call, state)
                          })

  result <- do.call(rbind, lapply(faceted.df, function(a) a$data))
  attr(result, "sort.x") <- lapply(faceted.df, function(ll) attr(ll$data, "sort.x"))
  attr(result, "sort.y") <- lapply(faceted.df, function(ll) attr(ll$data, "sort.y"))

  list(data = result,
       x.expr = faceted.df[[1]]$x.expr,
       y.expr = faceted.df[[1]]$y.expr)
}


.jjplot.sort <- function(data, x.expr, y.expr,
                         x = NULL, y = NULL,
                         decreasing = FALSE,
                         fun = mean) {
  sort.x <- match.call()[["x"]]
  sort.y <- match.call()[["y"]]
  stopifnot(xor(is.null(sort.x), is.null(sort.y)))
  result <- list(data = data, x.expr = x.expr, y.expr = y.expr)
  if (!is.null(sort.y)) {
    eval.y <- eval(sort.y, data)
    stopifnot(is.factor(data$y))
    oo <- order(tapply(eval.y, data$y, fun), decreasing = decreasing)
    attr(result$data, "sort.y") <- levels(data$y)[oo]
  }
  result
}

.jjplot.density <- function(data, x.expr, y.expr) {
  dd <- density(data$x)
  result <- data.frame(x = dd$x, y = dd$y)
  list(data = .bind.attr.columns(result, data),
       x.expr = x.expr,
       y.expr = substitute(Density(x), list(x = x.expr)))
}
