### STATS ###
.is.stat <- function(layer.call) {
  return(is.call(layer.call) && exists(eval(paste('jjplot.stat',as.character(layer.call[[1]]),sep='.'))))
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

jjplot.stat.table <- function(data, x.expr, y.expr, log.y = FALSE) {
  ## FIXME: log.y?
  tt <- table(data$x)
  df <- data.frame(x = names(tt), y = as.numeric(tt))
  if (log.y) {
    df$y <- log10(df$y)
  }
  list(data=.bind.attr.columns(df, data),
       x.expr=x.expr, y.expr = substitute(Count(x), list(x=x.expr)))
}

jjplot.stat.hist <- function(state,
                             align = c("left", "right", "middle"),
                             breaks = 20,
                             density = TRUE,
                             log.y = FALSE) {
  align <- match.arg(align)

  h <- hist(state$data$x, breaks = breaks, plot = FALSE)
  if (align == "left") {
    hx <- h$breaks[-length(h$breaks)]
  } else if (align == "right") {
    hx <- h$breaks[-1]
  } else {
    hx <- h$mids
  }
  
  cs <- h$density
  if (!density) {
    cs <- cs * length(state$data$x)
  }
  ## FIXME: log.y
  if (log.y) {
    dens <- log10(cs)
  } else {
    dens <- cs
  }
  state$data <- .bind.attr.columns(data.frame(x = hx, y = dens), state$data)
  state$y.expr <- substitute(Count(x), list(x = state$x.expr))
  state
}

jjplot.stat.jitter <- function(state,
                               xfactor = 0, yfactor = 0) {
  state$data <- transform(state$data,
                    x = jitter(as.numeric(state$data$x), xfactor))
  if (!is.null(state$data$y)) {
    state$data <- transform(state$data,
                      y = jitter(as.numeric(state$data$y), yfactor))    
  }
  if (xfactor != 0) {
    state$x.expr <- substitute(jitter(x), list(x=state$x.expr))
  }
  if (yfactor != 0) {
    state$y.expr <- substitute(jitter(x), list(x=state$y.expr))
  }
  state
}

jjplot.stat.fit <- function(state) {
  model <- lm(state$data$y ~ state$data$x)
  result <- data.frame(b = coef(model)[1],
                       a = coef(model)[2])
  state$data <- .bind.attr.columns(result, state$data)
  state
}

jjplot.stat.fun.x <- function(state, fun) {
  state$data <- .bind.attr.columns(data.frame(x = fun(state$data$x)), state$data)
  state
}

jjplot.stat.fun.y <- function(state, fun) {
  state$data <- .bind.attr.columns(data.frame(y = fun(state$data$y)), state$data)
  state
}

jjplot.stat.quantile <- function(data, x.expr, y.expr) {
  stopifnot(all(data$x == data$x[1]))
  result <- data.frame(data$x[1], t(quantile(data$y)))
  colnames(result) <- c("x", "quantile.0", "quantile.25", "quantile.50", "quantile.75", "quantile.100")
  rownames(result) <- NULL

  list(data = .bind.attr.columns(result, data),
       x.expr = x.expr,
       y.expr = y.expr)
}

jjplot.stat.ccdf <- function(data, x.expr, y.expr,
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

jjplot.stat.cumsum <- function(data, x.expr, y.expr,
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

jjplot.stat.group <- function(state,
                              fun, by) {
  eval.by <- eval(match.call()$by, state$data)  
  fun.call <- match.call()$fun

  faceted.df <- base:::by(cbind(state$data, .by = eval.by),
                          eval.by,
                          function(df) {
                            local.state <- list(data = df,
                                                x.expr = state$x.expr,
                                                y.expr = state$y.expr)
                            result <- .call.with.data(fun.call, local.state)
                          })

  result <- do.call(rbind, lapply(faceted.df, function(a) a$data))

  

##   attr(result, "sort.x") <- do.call(rbind,
##                                     lapply(faceted.df,
##                                            function(ll) attr(ll$data, "sort.x")))
##   if (!is.null(attr(result, "sort.x"))) {
##     attr(result, "sort.x") <- cbind(attr(result, "sort.x"),
##                                     .by = rep(names(faceted.df),
##                                         times = lapply(faceted.df, function(ll) {
##                                           nrow(attr(ll$data, "sort.x"))
##                                         })))
##     rownames(attr(result, "sort.x")) <- NULL
##   }

##   attr(result, "sort.y") <- do.call(rbind,
##                                     lapply(faceted.df,
##                                            function(ll) attr(ll$data, "sort.y")))
##   if (!is.null(attr(result, "sort.y"))) {
##     attr(result, "sort.y") <- cbind(attr(result, "sort.y"),
##                                     .by = rep(names(faceted.df),
##                                         times = lapply(faceted.df, function(ll) {
##                                           nrow(attr(ll$data, "sort.y"))
##                                         })))
##     rownames(attr(result, "sort.y")) <- NULL
##   }

  state$data <- result
  state$x.expr <- faceted.df[[1]]$x.expr
  state$y.expr <- faceted.df[[1]]$y.expr
  state
}


jjplot.stat.sort <- function(data, x.expr, y.expr,
                             x = NULL, y = NULL,
                             decreasing = FALSE,
                             fun = mean) {
  sort.x <- match.call()[["x"]]
  sort.y <- match.call()[["y"]]
  stopifnot(xor(is.null(sort.x), is.null(sort.y)))
  result <- list(data = data, x.expr = x.expr, y.expr = y.expr)
  if (!is.null(sort.x)) {
    eval.x <- eval(sort.x, data)
    stopifnot(is.factor(data$x))
    oo <- order(tapply(eval.x, data$x, fun), decreasing = decreasing)
    attr(result$data, "sort.x") <- data.frame(value = levels(data$x)[oo])
  }
  if (!is.null(sort.y)) {
    eval.y <- eval(sort.y, data)
    stopifnot(is.factor(data$y))
    oo <- order(tapply(eval.y, data$y, fun), decreasing = decreasing)
    attr(result$data, "sort.y") <- data.frame(value = levels(data$y)[oo])
  }
  result
}

jjplot.stat.color <- function(state,
                              color.expression,
                              alpha = 1.0,
                              manual = NULL) {
  state$data$color <- eval(match.call()$color.expression, state$data)
  state$scales$color <- .make.color.scale(state$data$color, alpha, manual)
  state
}

jjplot.stat.density <- function(state) {
  dd <- density(as.numeric(state$data$x))
  result <- data.frame(x = dd$x, y = dd$y)
  state$data <- .bind.attr.columns(result, state$data)
  state$y.expr <- substitute(Density(x), list(x = state$x.expr))
  state
}
