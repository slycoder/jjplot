#!Rscript

require("grid")

### SCALES ###
match.scale <- function(override, group, scales,
                        defaults = list(color = "black",
                                        size = 1.0,
                                        fill = "black"),
                        type = c("color", "size", "fill")) {
  type <- match.arg(type)
  if (!is.null(override)) {
    if (is.factor(override)) {
      scales[[type]](override)
    } else {
      override
    }
  } else if (is.null(group)) {
    defaults[[type]]
  } else {
    scales[[type]](group)
  }
}

make.color.scale <- function(cc, alpha) {
  if (is.null(cc)) {
    function(z) { rgb(0, 0, 0, alpha) }
  } else if (is.factor(cc)) {
    colors <- hcl(h = seq(0, 360, length.out = nlevels(cc) + 1),
                  l = 75,
                  c = 55,
                  alpha = alpha)
    colors <- colors[-length(colors)]
    function(z) { colors[z] }
  } else {
    cr <- colorRamp(c('red', 'white', 'blue'))
    rr <- range(cc)
    function(z) {
      z[z > rr[2]] <- rr[2]
      z[z < rr[1]] <- rr[1]
      rgb(cr((z  - rr[1]) / (rr[2] - rr[1])),
          alpha = alpha * 255,
          maxColorValue = 255)
    }
  }
}

make.size.scale <- function(ss) {
  if (is.null(ss)) {
    function(z) { 1.0 }
  } else {
    min.size <- 0.8
    max.size <- 3.3
    num.sizes <- 10
    size.levels <- seq(sqrt(min.size), sqrt(max.size), length.out = num.sizes)^2
    rr <- range(ss)
    function(z) { size.levels[round(num.sizes * (z - rr[1]) / (rr[2] - rr[1]))] }
  }          
}

### GEOMS ###
geoms.list <- c("hline",
                "vline",
                "abline",
                "point",
                "line",
                "text",
                "bar",
                "box")

is.geom <- function(layer.call) {
  return(is.call(layer.call) && as.character(layer.call[[1]]) %in% geoms.list)
}

jjplot.line <- function(data,
                        x.expr, y.expr,
                        lty = "solid", col = NULL,
                        lwd = 1.5, ordered = TRUE) {
  if (ordered) {
    oo <- order(data$x)
  } else {
    oo <- 1:nrow(data)
  }
  if (!is.null(data$color) && is.null(col))  {
    by(data[oo,], data$color[oo],
       function(zz)
       grid.lines(x = zz$x, y = zz$y,
                  default.units = "native",
                  gp = gpar(col = match.scale(col, zz$color, scales),
                    lwd = lwd,
                    lty = lty)))
  } else {
    grid.lines(x = data$x[oo], y = data$y[oo],
               default.units = "native",
               gp = gpar(col = match.scale(col, data$color[oo], scales),
                 lwd = lwd,
                 lty = lty))
  }
}

jjplot.bar <- function(data, x.expr, y.expr,
                       col = NULL, fill = NULL, width = 1) {
  grid.rect(data$x,
            0,
            width,
            data$y,
            just = c("center", "bottom"),
            default.units = "native",
            gp = gpar(fill = match.scale(fill, data$fill, type = "fill"), 
              col = match.scale(col, data$color, scales)))
}

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
         x = jitter(as.numeric(data$x), xfactor),
         y = jitter(as.numeric(data$y), yfactor)),
       x.expr = if (xfactor != 0) substitute(jitter(x), list(x=x.expr)) else x.expr,
       y.expr = if (yfactor != 0) substitute(jitter(x), list(x=y.expr)) else y.expr)
}
    

### UTILITY FUNCTIONS ###
call.with.data <- function(cc, state) {
  do.call(paste("jjplot", as.character(cc[[1]]), sep = "."),
          c(as.list(cc)[-1],
            list(data = state$data),
            x.expr = quote(state$x.expr),
            y.expr = quote(state$y.expr)))
}
## Memoization can take one of two values:
## * NULL - no memoization.  All stats should be computed and stored in memoization
## * list - memoization has already happened.  Stats should *not* be computed.
.formula.apply.helper <- function(expr,
                                  stat.function,
                                  geom.function,
                                  state,
                                  memoization = NULL) {
  operator <- expr[[1]]
  ret.memoization <- list()
  if (operator == ":") {
    stopifnot(is.stat(expr[[3]]))
    if (is.null(memoization)) {
      state <- stat.function(expr[[3]], state)
      ret.memoization <- c(ret.memoization, list(state))
    } else {
      state <- memoization[[1]]
    }
    ret <- .formula.apply.helper(expr[[2]], stat.function, geom.function, state, memoization[-1])
    ret.memoization <- c(ret.memoization, ret)
  } else if (operator == "+") {
    ret1 <- .formula.apply.helper(expr[[3]], stat.function, geom.function, state, memoization)
    ret2 <- .formula.apply.helper(expr[[2]], stat.function, geom.function, state, memoization)
    ret.memoization <- c(ret.memoization, ret1, ret2)
  } else if (operator == "(") {
    ret <- .formula.apply.helper(expr[[2]], stat.function, geom.function, state, memoization)
    ret.memoization <- c(ret.memoization, ret)
  } else if (is.geom(expr)) {
    geom.function(expr, state)
  } else {
    stop(paste("Invalid operator", operator))
  }
  return(ret.memoization)
}

formula.apply <- function(f,
                          stat.function,
                          geom.function,
                          data,
                          memoization = NULL) {
  y.expr <- f[[2]]
  rhs <- f[[3]]
  stopifnot(rhs[[1]] == "+")
  x.expr <- rhs[[3]]
  if (is.null(memoization)) {
    state <- list(data = data.frame(x = eval(x.expr, data),
                    y = eval(y.expr, data)),
                  x.expr = x.expr,
                  y.expr = y.expr)
  } else {
    state <- memoization[[1]]
  }
  
  ret <- .formula.apply.helper(rhs[[2]], stat.function, geom.function, state, memoization[-1])
  if (is.null(memoization)) {
    c(list(state), ret)
  } else {
    NULL
  }
}

.get.plot.params <- function(f, stats, log.x, log.y, expand, .subset = NULL) {
  ## Length-2 numerics.
  xrange <- NULL
  yrange <- NULL

  ## Either a character vector of levels, or FALSE.
  x.is.factor <- NULL
  y.is.factor <- NULL

  ## Expressions.
  xlab <- NULL
  ylab <- NULL
  
  ## Expands a range to incorporate new.data.
  expand.range <- function(old.range, new.data) {
    if (is.null(old.range) && is.null(new.data)) {
      return(NULL)
    }
    if (is.factor(new.data)) {
      range(c(0.5, nlevels(new.data) + 0.5, old.range))
    } else {
      range(c(old.range, new.data))
    }
  }

  update.range <- function(expr, state) {
    ## FIXME: geom.expansion
    ##    extra.data <- geom.expansion(layer.call)      
    xrange <<- expand.range(xrange, state$data$x)
    yrange <<- expand.range(yrange, state$data$y)

    if (is.null(xlab)) {
      xlab <<- state$x.expr
    }
    if (is.null(ylab)) {
      ylab <<- state$y.expr
    }
    
    if (is.null(x.is.factor)) {
      if (is.factor(state$data$x)) {        
        x.is.factor <<- levels(state$data$x)
      } else {
        x.is.factor <<- FALSE
      }
    }
    if (is.null(y.is.factor)) {
      if (is.factor(state$data$y)) {        
        y.is.factor <<- levels(state$data$y)
      } else {
        y.is.factor <<- FALSE
      }
    }
  }
  
  formula.apply(f, function(...) NULL,
                update.range,
                data, stats)
  
  if (is.character(x.is.factor)) {
    pretty.x <- length(x.is.factor)
    labels.x <- x.is.factor
    label.x.height <- convertHeight(unit(1, "strheight",
                                         labels.x[which.max(nchar(labels.x))]),
                                    "lines", valueOnly = TRUE) + 2.1
  } else {
    pretty.x <- pretty(xrange)
    labels.x <- TRUE
    label.x.height <- 3.1
    ## FIXME: manual labeling
    ##    if (!is.null(calls$labels.x)) {
    ##      labels.x <- eval(calls$labels.x, parent.frame(n = 4))
    ##      label.x.height <- convertHeight(unit(1, "strheight", labels.x[which.max(nchar(labels.x))]),
    ##                                      "lines", valueOnly = TRUE) + 2.1
    ##    }
  }

  if (log.x) {
    labels.x <- sapply(pretty.x, function(x)
                       substitute(10^x, list(x = x)),
                       simplify=FALSE)
    labels.x <- do.call(expression, labels.x)
  }

  if (is.character(y.is.factor)) {
    ## Figure how wide the text is going to be:
    if (squash.unused) {
      y.is.factor <- factor(y.is.factor)
      yrange <- c(1, length(y.is.factor))
    }
    pretty.y <- 1:length(y.is.factor)
    labels.y <- y.is.factor
  } else {
    pretty.y <- pretty(yrange)
    labels.y <- prettyNum(pretty.y)
  }
  
  if (log.y) {
    labels.y <- sapply(pretty.y, function(x)
                       substitute(10^x, list(x = x)),
                       simplify=FALSE)
    labels.y <- do.call(expression, labels.y)
  }
  
  label.y.width <- convertWidth(unit(1, "strwidth",
                                     labels.y[which.max(nchar(labels.y))]),
                                "lines", valueOnly = TRUE)

  xrange <- range(c(xrange, pretty.x))    
  yrange <- range(c(yrange, pretty.y))
  
  xrange[1] <- xrange[1] - (xrange[2] - xrange[1]) * expand[1]
  xrange[2] <- xrange[2] + (xrange[2] - xrange[1]) * expand[1]
  
  yrange[1] <- yrange[1] - (yrange[2] - yrange[1]) * expand[2]
  yrange[2] <- yrange[2] + (yrange[2] - yrange[1]) * expand[2]
  
  list(xrange = xrange, yrange = yrange,
       pretty.x = pretty.x, pretty.y = pretty.y,
       labels.x = labels.x, labels.y  = labels.y,
       xlab = xlab, ylab = ylab,
       label.x.height = label.x.height, label.y.width = label.y.width)
}


## Goes through the formula tree ONCE.
## Data can be subsetted by .subset.
.subplot <- function(f, stats, plot.params,
                     .subset = NULL,
                     draw.x.axis = TRUE,
                     draw.y.axis = TRUE,
                     allocate.x.axis.space = TRUE) {
  if (draw.x.axis && allocate.x.axis.space) {
    xmargin <- plot.params$label.x.height
  } else {
    xmargin <- 0
  }
  
  if (!is.null(plot.params$title)) {
    titlemargin <- 2.1
  } else {
    titlemargin <- 0.4
  }

  ## Set up viewport and draw grill.
  pushViewport(plotViewport(c(xmargin, plot.params$label.y.width + 1.5, titlemargin, 1.1)))
  pushViewport(dataViewport(xscale = plot.params$xrange,
                            yscale = plot.params$yrange))
  grid.rect(gp = gpar(fill = "grey90", col = "white"))
  grid.grill(plot.params$pretty.y, plot.params$pretty.x,
             gp = gpar(col = "white", lwd = 1.5),
             default.units = "native")
  
  midpoints <- function(v) {
    (v[-1] + v[-length(v)]) / 2
  }
      
  grid.grill(midpoints(plot.params$pretty.y), midpoints(plot.params$pretty.x),
             gp = gpar(col = "white", lwd = 0.5),
             default.units = "native")

  ## Do the actual plotting!
  formula.apply(f, function(...) NULL,
                call.with.data,
                data, stats)

  ## FIXME: Squash unused.
  ## Axes and labels.
  if (draw.x.axis) {
    grid.xaxis(at = plot.params$pretty.x,
               label = plot.params$labels.x,
               gp = gpar(col = "grey50", cex = 0.8))
    print(plot.params$xlab)
    grid.text(plot.params$xlab,
              y = unit(-plot.params$label.x.height + 0.5, "lines"),
              gp = gpar(col = "grey20", cex= 0.9))
  }
  if (draw.y.axis) {
    grid.yaxis(at = plot.params$pretty.y,
               label = plot.params$labels.y,
               gp = gpar(col = "grey50", cex = 0.8))
    print(plot.params$ylab)
    grid.text(plot.params$ylab,
              x = unit(-plot.params$label.y.width - 1.5, "lines"), rot = 90,
              gp = gpar(col = "grey20", cex= 0.9))
  }

  grid.text(plot.params$title, y = unit(2, "lines"))

  ## Draw a right-hand strip.
  if (!is.null(.subset)) {
    x.left <- convertX(unit(1.0, "npc"), "native", valueOnly=TRUE)
    y.lim <- convertY(unit(c(0, 1), "npc"), "native", valueOnly=TRUE)
    
    grid.rect(x.left, (y.lim[2] + y.lim[1]) / 2,
              unit(.9, "lines"), y.lim[2] - y.lim[1],
              hjust = 0,
              default.units = "native",
              gp = gpar(fill = "grey70"))
    
    subset.label <- .subset
    if (!is.null(calls$grid.y.labeler)) {
      subset.label <- eval(calls$grid.y.labeler)(.subset)
    }
    
    grid.text(x = x.left, y = (y.lim[2] + y.lim[1]) / 2,
              vjust = -0.5,
              label = subset.label, rot = 270,
              default.units = "native", gp = gpar(cex = 0.8))
  }

  popViewport(2)
}

### ENTRY POINT ###
jjplot <- function(f, data = NULL, color = NULL,
                   fill = NULL, size = NULL, alpha = 1.0,
                   log.x = FALSE, log.y = FALSE,
                   expand = c(0.04, 0.04),
                   ...) {

  ## Create the scales.
  eval.color <- eval(match.call()$color, data)
  eval.fill <- eval(match.call()$fill, data)
  eval.size <- eval(match.call()$size, data)

  scales <- list()
  scales$color <- make.color.scale(eval.color, alpha)
  scales$fill <- make.color.scale(eval.fill, alpha)
  scales$size <- make.size.scale(eval.size)    

  ## Compute stats.
  stats <- formula.apply(f, call.with.data, function(...) NULL, data)

  ## Compute plotting parameters.
  plot.params <- .get.plot.params(f, stats, log.x, log.y, expand)

  grid.newpage()
  ## Do the plot.
  .subplot(f, stats, plot.params)
}

### DEPRECATED ###
jjplot.old <-
  function(x,
           y = NULL,
           data,
           ...) {
    op <- par(no.readonly = TRUE)

    # flag whether x or y axis is to be log-transformed
    log.arg = match.call()$log
    log.x <- (!is.null(log.arg) && (log.arg == 'x' || log.arg == 'xy'))
    log.y <- (!is.null(log.arg) && (log.arg == 'y' || log.arg == 'xy'))
    eval.x <- eval(match.call()[["x"]], data)
    ylab.default <- NULL
    
    if(log.x && is.numeric(eval.x)) {
      eval.x <- log10(eval.x)
    }

    color.expr <- match.call()$color
    fill.expr <- match.call()$fill
    size.expr <- match.call()$size

    facet.data <- NULL
    layer.data <- NULL

    squash.unused <- if (is.null(match.call()$squash.unused)) FALSE else eval(match.call()$squash.unused)
    
    jjplot.group <- function(f, by = NULL) {
      eval.facet <- eval(match.call()$by, data)
      stopifnot(!is.null(eval.facet))

      facet.call <- match.call()$by
      f.call <- match.call()$f
      
      faceted.df <- base:::by(cbind(facet.data, .facet = eval.facet),
                       eval.facet,
                       function(df) { facet.data <<- df
                                      result <- eval(f.call)
                                      if (!is.null(fill.expr) && facet.call == fill.expr)
                                        result$fill <- df$.facet[1]
                                      if (!is.null(color.expr) && facet.call == color.expr)
                                        result$color <- df$.facet[1]
                                      if (!is.null(size.expr) && facet.call == size.expr)
                                        result$size <- df$.facet[1]
                                      result$.facet <- df$.facet[1]
                                      result
                                    })
      do.call(rbind, faceted.df)
    }

    ## Stats
    
    jjplot.fun.x <- function(f) {
      data.frame(x = f(facet.data$x))
    }

    jjplot.fun.y <- function(f) {
      data.frame(y = f(facet.data$y))
    }

    jjplot.fit <- function() {
      model <- lm(facet.data$y ~ facet.data$x)
      data.frame(b = coef(model)[1],
                 a = coef(model)[2])
    }

    jjplot.quantile <- function() {
      stopifnot(all(facet.data$x == facet.data$x[1]))
      result <- data.frame(facet.data$x[1], t(quantile(facet.data$y)))
      colnames(result) <- c("x", "quantile.0", "quantile.25", "quantile.50", "quantile.75", "quantile.100")
      rownames(result) <- NULL
      result
    }

    jjplot.ccdf <- function(density=FALSE,maxpoints=FALSE) {
      freqs = table(facet.data$x)
      df <- data.frame(x=as.numeric(rev(names(freqs))),y=cumsum(rev(freqs)))
      if(density) {
      	df$y <- df$y/nrow(df)
      	ylab.default <<- substitute(Pr(x>=X),list(x=x.expr,X=toupper(x.expr)))
      } else {
      	ylab.default <<- substitute(Count(x>=X),list(x=x.expr,X=toupper(x.expr)))
      }
      if (log.y) {
      	df$y <- log10(df$y)
      }
      if (log.x && maxpoints != FALSE && is.numeric(maxpoints)) {
        group = cut(df$x, b=maxpoints)
        df <- do.call(rbind, by(df, group,
          function(X) X[order(X$x)[floor(length(X)/2)],]))
      }
      
      df
    }

    jjplot.cumsum <- function(decreasing=TRUE) {
      oo <- order(facet.data$x, decreasing=decreasing)
      ylab.default <<- substitute(cumsum(x), list(x = y.expr))
      cs <- cumsum(facet.data$y[oo])
      if (log.y) {
      	cs <- log10(cs)
      }      
      data.frame(x = facet.data$x[oo], y = cs)
    }
    
    ## Geoms    
    jjplot.hline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      grid.lines(y = layer.data$y,
                 default.units = "native",
                 gp = gpar(col = match.colors(col, layer.data$color),
                   lwd = lwd,
                   lty = lty))
    }    
    
    jjplot.vline <- function(manual.x = NULL, lwd = 1.5, col = NULL, lty = "solid") {
      grid.lines(x = if (is.null(manual.x)) layer.data$x else manual.x,
                 default.units = "native",
                 gp = gpar(col = match.colors(col, layer.data$color),
                   lwd = lwd,
                   lty = lty))
    }

    jjplot.abline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      ## Find limits
      xlim <- convertX(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)
      ylim <- convertY(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)      

      ystart <- xlim[1] * layer.data$a + layer.data$b
      yend <- xlim[2] * layer.data$a + layer.data$b

      xstart <- ifelse(layer.data$a < 0,
                       (ylim[2] - layer.data$b) / layer.data$a,
                       (ylim[1] - layer.data$b) / layer.data$a)
      xend <- ifelse(layer.data$a < 0,
                     (ylim[1] - layer.data$b) / layer.data$a,
                     (ylim[2] - layer.data$b) / layer.data$a)

      ## Invariant: xstart <= xend,
      ## So that left hand coordinate should be xtart or xlim[1]
      ## and right hand should be xend or xlim[2].      

      ystart <- ifelse(xstart < xlim[1],
                       ystart,
                       ifelse(layer.data$a < 0,
                              ylim[2],
                              ylim[1]))

      yend <- ifelse(xend > xlim[2],
                     yend,
                     ifelse(layer.data$a < 0,
                            ylim[1],
                            ylim[2]))

      xstart <- pmax(xstart, xlim[1])
      xend <- pmin(xend, xlim[2])
      
      grid.segments(xstart, ystart, xend, yend,
                    default.units = "native",
                    gp = gpar(lwd = lwd,
                      lty = lty,
                      col = match.colors(col, layer.data$color)))
    }
    
    jjplot.point <- function(pch = 16, col = NULL, size = NULL) {
      grid.points(layer.data$x,
                  layer.data$y,
                  pch = pch,
                  size = unit(0.5 * match.sizes(size, layer.data$size), "char"),
                  gp = gpar(alpha = eval.alpha,
                    col = match.colors(col, layer.data$color),
                    fill = match.colors(col, layer.data$fill, use.fill = TRUE)))
    }

    jjplot.text <- function(col = NULL, label = NULL,
                            x = NULL, y = NULL, hjust = 0.5,
                            vjust = 0.5) {
      if (is.null(x)) {
        x <- layer.data$x
      }
      if (is.null(y)) {
        y <- layer.data$y
      }
      if (is.null(label)) {
        label <- layer.data$label
      }
      grid.text(label = label, x = x, y = y, 
                hjust = hjust, vjust = vjust,
                default.unit = "native",
                gp = gpar(col = match.colors(col, layer.data$color)))
    }

    jjplot.box <- function(col = NULL, fill = NULL, width = 0.5,
                         lwd = 1.5, lty = "solid") {
      grid.rect(as.numeric(layer.data$x),
                layer.data$quantile.25,
                width,
                layer.data$quantile.75 - layer.data$quantile.25,
                default.units = "native",
                just = c("center", "bottom"),
                gp = gpar(lwd = lwd,
                  lty = lty,
                  fill = match.colors(fill, layer.data$fill, use.fill = TRUE),
                  col = match.colors(col, layer.data$color)))

      grid.segments(c(layer.data$x, layer.data$x, as.numeric(layer.data$x) - width / 2),
                    c(layer.data$quantile.0, layer.data$quantile.100, layer.data$quantile.50),
                    c(layer.data$x, layer.data$x, as.numeric(layer.data$x) + width / 2),
                    c(layer.data$quantile.25, layer.data$quantile.75, layer.data$quantile.50),
                    default.units = "native",
                    gp = gpar(lwd = lwd,
                      lty = lty,
                      col = match.colors(col, layer.data$color)))
    }

    geom.expansion <- function(.call) {
      if (is.call(.call) && as.character(.call[[1]]) == "jjplot.bar") {
        width <- 1.0
        if (!is.null(.call$width)) {
          width <- .call$width
        }
        xlim <- range(as.numeric(layer.data$x))
        x.padding <- (xlim[2] - xlim[1]) * eval.expand[1]
        if (x.padding >= width / 2) {
          list(x = c(xlim[1] - width / 2 + x.padding,
                     xlim[2] + width / 2 - x.padding), y = 0)
        } else {
          list(y = 0)          
        }
      } else if (is.call(.call) && as.character(.call[[1]]) == "jjplot.box") {
        list(y = c(layer.data$quantile.0, layer.data$quantile.100))
      } else {
        list()
      }
    }
        
    eval.grid.x <- eval(match.call()$facet.x, data)
    eval.grid.y <- eval(match.call()$facet.y, data)
    stopifnot(is.null(eval.grid.x))
    

    if (is.null(eval.grid.y)) {
      ..subplot(match.call(), .get.plot.range(match.call()))
    } else {
      calls <- match.call()
      plot.params <- lapply(1:nlevels(eval.grid.y), function(ll)
                            .get.plot.range(calls, .subset = levels(eval.grid.y)[ll]))
      heights <- sapply(plot.params, function(pp) pp$yrange[2] - pp$yrange[1])
      top.vp <- viewport(layout = grid.layout(nlevels(eval.grid.y) + 1, 1,
                         heights = unit.c(unit(heights, "null"),
                                          unit(plot.params[[1]]$label.x.height, "lines"))))
      
      subplots <- list()
      for (ll in 1:nlevels(eval.grid.y)) {
        subplots[[ll]] <- viewport(name = paste(".subplot", ll, sep = "."),
                                   layout.pos.row = ll, layout.pos.col = 1)
      }
      subplots[[nlevels(eval.grid.y) + 1]] <- viewport(name = "xaxis",
                                                       layout.pos.row = nlevels(eval.grid.y) + 1,
                                                       layout.pos.col = 1)
      pushViewport(vpTree(top.vp, do.call(vpList, subplots)))
      for (ll in 1:nlevels(eval.grid.y)) {
        seekViewport(paste(".subplot", ll, sep = "."))
        cat("Doing facet ")
        print(ll)
        ..subplot(match.call(),
                  plot.params[[ll]],
                  .subset = levels(eval.grid.y)[ll],
                 draw.x.axis = ll == nlevels(eval.grid.y), allocate.x.axis.space = FALSE)
      }
      popViewport()
    }
}

