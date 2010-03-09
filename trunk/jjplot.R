#!Rscript

require("grid")

source("scales.R")
source("stats.R")
source("geoms.R")

### UTILITY FUNCTIONS ###
.call.with.data <- function(cc, state, prefix = ".jjplot", ...) {
  do.call(paste(prefix, as.character(cc[[1]]), sep = "."),
          c(as.list(cc)[-1],
            list(data = state$data),
            x.expr = quote(state$x.expr),
            y.expr = quote(state$y.expr),
            ...))
}

## Memoize:
## * if True, memoization will take place.
## * if False, memoization will be used.
## Memoization can take one of two values:
## * NULL - no memoization.  All stats should be computed and stored in memoization
## * list - memoization has already happened.  Stats should *not* be computed.
.formula.apply.helper <- function(expr,
                                  stat.function,
                                  geom.function,
                                  state,
                                  memoize = TRUE,
                                  memoization = NULL) {
  if (!memoize) {
    stopifnot(!is.null(memoization))
  }
  operator <- expr[[1]]
  ret.memoization <- list()
  if (operator == ":") {
    stopifnot(.is.stat(expr[[3]]))
    if (memoize) {
      state <- stat.function(expr[[3]], state)
      ret.memoization <- c(ret.memoization, list(state))
    } else {
      state <- memoization[[1]]
    }
    ret <- .formula.apply.helper(expr[[2]], stat.function, geom.function, state, memoize, memoization[-1])
    if (memoize) {
      ret.memoization <- c(ret.memoization, ret)
    } else {
      memoization <- ret
    }
  } else if (operator == "+") {
    ret1 <- .formula.apply.helper(expr[[3]], stat.function, geom.function, state, memoize, memoization)
    if (!memoize) {
      memoization <- ret1
    }
    ret2 <- .formula.apply.helper(expr[[2]], stat.function, geom.function, state, memoize, memoization)
    if (memoize) {
      ret.memoization <- c(ret.memoization, ret1, ret2)      
    } else {
      memoization <- ret2
    }
  } else if (operator == "(") {
    ret <- .formula.apply.helper(expr[[2]], stat.function,
                                 geom.function, state, memoize, memoization)
    if (memoize) {
      ret.memoization <- c(ret.memoization, ret)
    } else {
      memoization <- ret
    }
  } else if (.is.geom(expr)) {
    geom.function(expr, state)
  } else {
    stop(paste("Invalid operator", operator))
  }
  if (memoize) {
    return(ret.memoization)
  } else {
    return(memoization)
  }
}

.formula.apply <- function(f,
                          stat.function,
                          geom.function,
                          data,
                          memoization = NULL,
                          color = NULL, fill = NULL, size = NULL) {
  if (length(f) == 3) {
    y.expr <- f[[2]]
    rhs <- f[[3]]
  } else if (length(f) == 2) {
    y.expr <- NULL
    rhs <- f[[2]]
  } else {
    stop("Malformed formula.")
  }
  
  stopifnot(rhs[[1]] == "+")
  x.expr <- rhs[[3]]
  if (is.null(memoization)) {
    eval.x <- eval(x.expr, data)
    eval.y <- eval(y.expr, data)
    data$x <- eval.x

    state <- list(data = data,
                  x.expr = x.expr,
                  y.expr = y.expr)
    
    if (!is.null(eval.y)) {
      state$data$y <- eval.y
    }
    if (!is.null(color)) {
      state$data$color <- color
    }
    if (!is.null(fill)) {
      state$data$fill <- fill
    }
    if (!is.null(size)) {
      state$data$size <- size
    }
  } else {
    state <- memoization[[1]]
  }
  
  ret <- .formula.apply.helper(rhs[[2]], stat.function, geom.function, state, is.null(memoization), memoization[-1])
  if (is.null(memoization)) {
    c(list(state), ret)
  } else {
    NULL
  }
}

.get.plot.params <- function(f, stats, log.x, log.y, expand,
                             xlab = NULL, ylab = NULL,
                             .subset = NULL, squash.unused = FALSE) {
  ## Length-2 numerics.
  xrange <- NULL
  yrange <- NULL

  ## Either a character vector of levels, or FALSE.
  x.is.factor <- NULL
  y.is.factor <- NULL

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
    if (exists(paste(".jjplot.expand", as.character(expr[[1]]), sep="."))) {
      expansion <- .call.with.data(expr, state, prefix = ".jjplot.expand")
    } else {
      expansion <- NULL
    }
    
    xrange <<- expand.range(xrange, c(state$data$x, expansion$x))
    yrange <<- expand.range(yrange, c(state$data$y, expansion$y))

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
  
  .formula.apply(f, function(...) NULL,
                 update.range,
                 data, stats)
  
  if (is.character(x.is.factor)) {
    pretty.x <- 1:length(x.is.factor)
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
                     scales,
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
  .formula.apply(f, function(...) NULL,
                 function(cc, state) {
                   .call.with.data(cc,
                                   state,
                                   scales = list(scales))
                 }, data, stats)
  
  ## FIXME: Squash unused.
  ## Axes and labels.
  if (draw.x.axis) {
    grid.xaxis(at = plot.params$pretty.x,
               label = plot.params$labels.x,
               gp = gpar(col = "grey50", cex = 0.8))
    grid.text(plot.params$xlab,
              y = unit(-plot.params$label.x.height + 0.5, "lines"),
              gp = gpar(col = "grey20", cex= 0.9))
  }
  if (draw.y.axis) {
    grid.yaxis(at = plot.params$pretty.y,
               label = plot.params$labels.y,
               gp = gpar(col = "grey50", cex = 0.8))
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

    ## FIXME!
    ##    if (!is.null(calls$grid.y.labeler)) {
    ##      subset.label <- eval(calls$grid.y.labeler)(.subset)
    ##    }
    
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
                   xlab = NULL, ylab = NULL,
                   expand = c(0.04, 0.04),
                   color.scale = NULL) {
  eval.color <- eval(match.call()$color, data)
  eval.fill <- eval(match.call()$fill, data)
  eval.size <- eval(match.call()$size, data)

  scales <- list()
  scales$color <- .make.color.scale(eval.color, alpha, manual = color.scale)
  scales$fill <- .make.color.scale(eval.fill, alpha)
  scales$size <- .make.size.scale(eval.size)    

  ## Compute stats.
  stats <- .formula.apply(f, .call.with.data, function(...) NULL, data,
                          color = eval.color, fill = eval.fill, size = eval.size)

  ## Compute plotting parameters.
  plot.params <- .get.plot.params(f, stats, log.x, log.y, expand,
                                  xlab = xlab, ylab = ylab)

  grid.newpage()
  ## Do the plot.
  .subplot(f, stats, plot.params, scales)
}

### DEPRECATED ###
.jjplot.old <-
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

    squash.unused <- if (is.null(match.call()$squash.unused)) FALSE else eval(match.call()$squash.unused)
    
    eval.grid.x <- eval(match.call()$facet.x, data)
    eval.grid.y <- eval(match.call()$facet.y, data)
    stopifnot(is.null(eval.grid.x))
    

    if (is.null(eval.grid.y)) {
      .subplot(match.call(), .get.plot.params(match.call()))
    } else {
      calls <- match.call()
      plot.params <- lapply(1:nlevels(eval.grid.y), function(ll)
                            .get.plot.params(calls, .subset = levels(eval.grid.y)[ll]))
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
        .subplot(match.call(),
                 plot.params[[ll]],
                 .subset = levels(eval.grid.y)[ll],
                 draw.x.axis = ll == nlevels(eval.grid.y), allocate.x.axis.space = FALSE)
      }
      popViewport()
    }
}

