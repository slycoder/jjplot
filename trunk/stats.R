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
  if (!is.null(data$border)) {
    if (all(data$border == data$border[1])) {
      result$border <- data$border[1]
    } 
  }
  if (!is.null(data$size)) {
    if (all(data$size == data$size[1])) {
      result$size <- data$size[1]
    } 
  }
  if (!is.null(data$shape)) {
    if (all(data$shape == data$shape[1])) {
      result$shape <- data$shape[1]
    } 
  }
  if (!is.null(data$alpha)) {
    if (all(data$alpha == data$alpha[1])) {
      result$alpha <- data$alpha[1]
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

jjplot.stat.log <- function(state,
                            ...) {
  sapply(match.call()[-(1:2)],
         function(colname) {
           colname <- as.character(colname)
           state$data[,colname] <<- log10(state$data[,colname])
           if (colname == "x") {
             state$x.expr <<- substitute(log[10](x), list(x = state$x.expr))
           } else if (colname == "y") {
             state$y.expr <<- substitute(log[10](x), list(x = state$y.expr))
           }
           0
         })
  state
}

jjplot.stat.table <- function(state) {
  tt <- table(state$data$x)
  df <- data.frame(x = names(tt), y = as.numeric(tt))
  state$data <- .bind.attr.columns(df, state$data)
  state$y.expr <- substitute(Count(x), list(x=state$x.expr))
  state
}

jjplot.stat.fun <- function(state,
                            fun,
                            stepsize=NULL,
                            sequence=NULL) {
  y <- sapply(sequence,fun)
  state$data <- .bind.attr.columns(data.frame(x=seq,y=y),
                                   state)
  state
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

jjplot.stat.quantile <- function(state) {
  state$data <-
    do.call(rbind,
            by(state$data, state$data$x, function(yy) {
              transform(cbind(yy[1,], .quantile = c(0, 25, 50, 75, 100),
                              row.names = NULL),
                        y = quantile(yy$y))
            }))
  state
}

jjplot.stat.ccdf <- function(state,
                             density = FALSE, maxpoints = FALSE,
                             log.y = FALSE, log.x = FALSE) {
  freqs <- table(state$data$x)
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

  state$data <- df
  if (density) {
    state$y.expr <- substitute(Pr(x>=X),list(x=state$x.expr,X=toupper(state$x.expr)))
  } else {
    state$y.expr <- substitute(Count(x>=X),list(x=state$x.expr,X=toupper(state$x.expr)))
  }
  state
}

jjplot.stat.cumsum <- function(state,
                               decreasing=TRUE,
                               log.y = FALSE) {
  oo <- order(state$data$x, decreasing=decreasing)
  cs <- cumsum(state$data$y[oo])
  if (log.y) {
    cs <- log10(cs)
  }
  state$data <- data.frame(x = data$x[oo], y = cs)
  state$y.expr <- substitute(cumsum(x), list(x = state$y.expr))
  state
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

  

  state$sort.x <- do.call(rbind,
                          lapply(faceted.df,
                                 function(ll) ll$sort.x))
  if (!is.null(state$sort.x)) {
    state$sort.y <- cbind(state$sort.x,
                          .by = rep(names(faceted.df),
                            times = lapply(faceted.df, function(ll) {
                              nrow(ll$sort.x)
                            })))
    rownames(state$sort.x) <- NULL
  }

  state$sort.y <- do.call(rbind,
                          lapply(faceted.df,
                                 function(ll) ll$sort.y))
  if (!is.null(state$sort.y)) {
    state$sort.y <- cbind(state$sort.y,
                          .by = rep(names(faceted.df),
                            times = lapply(faceted.df, function(ll) {
                              nrow(ll$sort.y)
                            })))
    rownames(state$sort.y) <- NULL
  }

  state$data <- result
  state$x.expr <- faceted.df[[1]]$x.expr
  state$y.expr <- faceted.df[[1]]$y.expr
  state
}


jjplot.stat.sort <- function(state,
                             x = NULL, y = NULL,
                             decreasing = FALSE,
                             fun = mean) {
  sort.x <- match.call()[["x"]]
  sort.y <- match.call()[["y"]]
  stopifnot(xor(is.null(sort.x), is.null(sort.y)))
  if (!is.null(sort.x)) {
    eval.x <- eval(sort.x, state$data)
    stopifnot(is.factor(state$data$x))
    oo <- order(tapply(eval.x, state$data$x, fun), decreasing = decreasing)
    state$sort.x <- data.frame(value = levels(state$data$x)[oo])
  }
  if (!is.null(sort.y)) {
    eval.y <- eval(sort.y, state$data)
    stopifnot(is.factor(state$data$y))
    oo <- order(tapply(eval.y, state$data$y, fun), decreasing = decreasing)
    state$sort.y <- data.frame(value = levels(state$data$y)[oo])
  }
  state
}

jjplot.stat.density <- function(state) {
  dd <- density(as.numeric(state$data$x))
  result <- data.frame(x = dd$x, y = dd$y)
  state$data <- .bind.attr.columns(result, state$data)
  state$y.expr <- substitute(Density(x), list(x = state$x.expr))
  state
}

jjplot.stat.color <- function(state,
                              color.expression,
                              alpha = 1.0,
                              manual = NULL) {
  state$data$color <- eval(match.call()$color.expression, state$data)
  state$scales$color <- .make.color.scale(state$data$color, alpha, manual)
  state
}

jjplot.stat.border <- function(state,
                               color.expression,
                               alpha = 1.0,
                               manual = NULL) {
  state$data$border <- eval(match.call()$color.expression, state$data)
  state$scales$border <- .make.color.scale(state$data$border, alpha, manual)
  state
}

jjplot.stat.size <- function(state,
                             size.expression,
                             manual = NULL) {
  stopifnot(is.null(manual))
  state$data$size <- eval(match.call()$size.expression, state$data)
  state$scales$size <- .make.size.scale(state$data$size)
  state
}

jjplot.stat.shape <- function(state,
                              shape.expression,
                              manual = NULL) {
  stopifnot(is.null(manual))  
  state$data$shape <- eval(match.call()$shape.expression, state$data)
  state$scales$shape <- .make.shape.scale(state$data$shape)
  state
}

jjplot.stat.alpha <- function(state,
                              alpha.expression,
                              manual = NULL) {
  stopifnot(is.null(manual))  
  state$data$alpha <- eval(match.call()$alpha.expression, state$data)
  state$scales$alpha <- .make.alpha.scale(state$data$alpha)
  state
}


jjplot.stat.fun.df <- function(state, fun) {
  state$data <- .bind.attr.columns(fun(state$data), state$data)
  state
}

jjplot.stat.fun.xy <- function(state,
                               x = NULL,
                               y = NULL,
                               ...) {  
  if (!is.null(x) && !is.null(y)) {
    f.x <- x(state$data$x, state$data$y, ...)
    f.y <- y(state$data$x, state$data$y, ...)
    state$data <- transform(state$data[1,], x = f.x, y = f.y)
  } else if (!is.null(x)) {
    f.x <- x(state$data$x, state$data$y, ...)
    state$data <- transform(state$data[1,], x = f.x)
  }
  else if (!is.null(y)) {
    f.y <- y(state$data$x, state$data$y, ...)
    state$data <- transform(state$data[1,], y = f.y)
  } else {
  }
  state
}

jjplot.stat.transform <- function(state,
                                  ...) {  
  state$data <- transform(state$data, ...)
  state
}


jjplot.stat.normalize <- function(state, col = 'y', ignore.zeros = T) {
  if (sum(state$data[,col]) != 0 || !ignore.zeros) {
    state$data[,col] <- state$data[,col] / sum(state$data[,col])
  }
  state
}
