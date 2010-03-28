#!Rscript

require("grid")
require("reshape")

source("scales.R")
source("stats.R")
source("geoms.R")

### UTILITY FUNCTIONS ###
.call.with.data <- function(cc, state, prefix = "jjplot.stat", ...) {
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
                           facet.x = NULL,
                           facet.y = NULL,
                           color = NULL,
                           fill = NULL,
                           size = NULL) {
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
    if (!is.null(facet.x)) {
      state$data$.facet.x <- facet.x
    }
    if (!is.null(facet.y)) {
      state$data$.facet.y <- facet.y
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

.try.subset <- function(df, ff) {
  if (is.null(ff) || is.null(df) || ncol(df) == 1) {
    return(df)
  }
  
  for (ii in 2:ncol(df)) {
    if (ff %in% levels(df[,ii])) {
      return(subset(df, df[,ii] == ff))
    }
  }
  warning("It looks like sorting was grouped, but none of the groups correspond to a facet!")
  return(df)
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
        sort.x <- attr(state$data, "sort.x", exact = TRUE)
        sort.x <- .try.subset(sort.x, .subset$facet.x)
        sort.x <- .try.subset(sort.x, .subset$facet.y)
        
        if (!is.null(sort.x)) {
          x.is.factor <<- as.character(sort.x[,1])
        } else {
          x.is.factor <<- levels(state$data$x)
        }
      } else {
        x.is.factor <<- FALSE
      }
    }
    if (is.null(y.is.factor)) {
      if (is.factor(state$data$y)) {
        sort.y <- attr(state$data, "sort.y", exact = TRUE)
        sort.y <- .try.subset(sort.y, .subset$facet.x)
        sort.y <- .try.subset(sort.y, .subset$facet.y)
        
        if (!is.null(sort.y)) {
          y.is.factor <<- as.character(sort.y[,1])
        } else {
          y.is.factor <<- levels(state$data$y)
        }
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
                                "lines", valueOnly = TRUE) + 2

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
                     draw.top.strip = NULL,
                     draw.right.strip = NULL,
                     allocate.x.axis.space = TRUE,
                     allocate.y.axis.space = TRUE) {  

  if (draw.x.axis && allocate.x.axis.space) {
    xmargin <- plot.params$label.x.height
  } else {
    xmargin <- 0
  }

  if (draw.y.axis && allocate.y.axis.space) {
    ymargin <- plot.params$label.y.width
  } else {
    ymargin <- 0
  }
  
#  if (!is.null(plot.params$title) || !is.null(draw.top.strip)) {
  if (!is.null(plot.params$title)) { 
    titlemargin <- 1.7
  } else {
    titlemargin <- 0.0
  }
  
  ## Set up viewport and draw grill.
  pushViewport(plotViewport(c(xmargin,
                              ymargin,
                              titlemargin + 1.1, 1.1)))
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
                   sort.x <- attr(state$data, "sort.x", exact = TRUE)
                   sort.y <- attr(state$data, "sort.y", exact = TRUE)

                   condition <- TRUE
                   if (!is.null(.subset) && !is.null(.subset$facet.x) &&
                       !is.null(state$data$.facet.x)) {
                     condition <- condition & (state$data$.facet.x == .subset$facet.x)
                   }

                   if (!is.null(.subset) && !is.null(.subset$facet.y) &&
                       !is.null(state$data$.facet.y)) {
                     condition <- condition & (state$data$.facet.y == .subset$facet.y) 
                   }
                   state$data <- subset(state$data, condition)

                   sort.y <- .try.subset(sort.y, .subset$facet.x)
                   sort.y <- .try.subset(sort.y, .subset$facet.y)

                   sort.x <- .try.subset(sort.x, .subset$facet.x)
                   sort.x <- .try.subset(sort.x, .subset$facet.y)

                   if (!is.null(sort.y) && !is.null(state$data$y)) {
                     state$data$y <- factor(state$data$y,
                                            levels = sort.y[,1])
                   }

                   if (!is.null(sort.x) && !is.null(state$data$x)) {
                     state$data$x <- factor(state$data$x,
                                            levels = sort.x[,1])
                   }

                   .call.with.data(cc,
                                   state,
                                   scales = list(scales),
                                   prefix = 'jjplot.geom')
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
              x = unit(-plot.params$label.y.width + .25, "lines"), rot = 90,
              gp = gpar(col = "grey20", cex= 0.9))
  }

  grid.text(plot.params$title, y = unit(2, "lines"))

  ## Draw a right-hand strip.
  if (!is.null(draw.right.strip)) {
    x.left <- convertX(unit(1.0, "npc"), "native", valueOnly=TRUE)
    y.lim <- convertY(unit(c(0, 1), "npc"), "native", valueOnly=TRUE)
    
    grid.rect(x.left, (y.lim[2] + y.lim[1]) / 2,
              unit(.9, "lines"), y.lim[2] - y.lim[1],
              hjust = 0,
              default.units = "native",
              gp = gpar(fill = "grey70"))
    
    grid.text(x = x.left, y = (y.lim[2] + y.lim[1]) / 2,
              vjust = -0.5,
              label = draw.right.strip, rot = 270,
              default.units = "native", gp = gpar(cex = 0.8))
  }

  if (!is.null(draw.top.strip)) {
    y.bottom <- convertY(unit(1.0, "npc"), "native", valueOnly=TRUE)
    x.lim <- convertX(unit(c(0, 1), "npc"), "native", valueOnly=TRUE)

    grid.rect((x.lim[2] + x.lim[1]) / 2, y.bottom,
              x.lim[2] - x.lim[1], unit(.9, "lines"), 
              vjust = 0,
              default.units = "native",
              gp = gpar(fill = "grey70"))
    
    grid.text(x = (x.lim[2] + x.lim[1]) / 2,
              y = y.bottom,
              vjust = -0.5,
              label = draw.top.strip, 
              default.units = "native", gp = gpar(cex = 0.8))
  }

  popViewport(2)
}

.faceted.plot <- function(f, stats, facet.x, facet.y,
                          facet.nrow, facet.ncol, scales, ...) {

  if (!is.null(facet.x) && !is.null(facet.y)) {
    stopifnot(is.na(facet.nrow) && is.na(facet.ncol))
    width <- nlevels(facet.x)
    height <- nlevels(facet.y)
    num.facets <- width * height
  } else {
    if (!is.null(facet.x)) {
      facet <- facet.x
      height <- 1
      width <- nlevels(facet)
    } else {
      facet <- facet.y
      width <- 1
      height <- nlevels(facet)
    }
    stopifnot(is.na(facet.nrow) || is.na(facet.ncol))
    if (!is.na(facet.nrow)) {
      height <- facet.nrow
      width <- ceiling(nlevels(facet) / height)
    } else if (!is.na(facet.ncol)) {
      width <- facet.ncol
      height <- ceiling(nlevels(facet) / width)
    }
    num.facets <- nlevels(facet)
  }

  get.facet.info <- function(index) {
    if (!is.null(facet.x) && !is.null(facet.y)) {
      x.index <- (index - 1) %% width + 1
      y.index <- (index - 1) %/% width + 1
      return(list(facet.x = levels(facet.x)[x.index],
                  facet.y = levels(facet.y)[y.index],
                  x = x.index, y = y.index,
                  top.strip = if (y.index == height) levels(facet.x)[x.index] else NULL,
                  right.strip = if (x.index == width) levels(facet.y)[y.index] else NULL))
    } else if (!is.null(facet.x)) {
      x.index <- (index - 1) %% width + 1
      y.index <- (index - 1) %/% width + 1
      return(list(facet.x = levels(facet.x)[index],
                  facet.y = NULL,
                  top.strip = levels(facet.x)[index],
                  right.strip = NULL,
                  x = x.index, y = y.index))
    } else {
      y.index <- (index - 1) %% height + 1
      x.index <- (index - 1) %/% height + 1
      return(list(facet.y = levels(facet.y)[index],
                  facet.x = NULL,
                  right.strip = levels(facet.y)[index],
                  top.strip = NULL,
                  x = x.index, y = y.index))
    }
  }
  
  cat("Facet dimensions: ")
  print(c(width, height))

  plot.params <- lapply(1:num.facets,
                        function(ll) {
                          .get.plot.params(f, stats, ...,
                                           .subset = get.facet.info(ll))
                        })

#  heights <- sapply(plot.params, function(pp) pp$yrange[2] - pp$yrange[1])
#  widths <- sapply(plot.params, function(pp) pp$xrange[2] - pp$xrange[1])
  heights <- rep(1, height)
  widths <- rep(1, width)

  top.vp <- viewport(layout = grid.layout(height + 1, width + 1,
                       heights = unit.c(
#                         unit(0.8, "lines"),
                         unit(heights, "null"),
                         unit(plot.params[[1]]$label.x.height, "lines")),
                       widths = unit.c(unit(plot.params[[1]]$label.y.width, "lines"),
                         unit(widths, "null"))))
  
  subplots <- list()
  for (ll in 1:num.facets) {
    facet.info <- get.facet.info(ll)
    subplots[[ll]] <- viewport(name = paste(".subplot", ll, sep = "."),
                               layout.pos.col = facet.info$x + 1,
                               layout.pos.row = height - facet.info$y + 1)
  }
##   for (jj in 1:width) {
##     subplots[[num.facets + jj]] <- viewport(name = paste("xaxis", jj, sep="."),
##                                             layout.pos.row = height + 1,
##                                             layout.pos.col = jj + 1)
##   }
##   for (jj in 1:(height + 1)) {
##     subplots[[num.facets + jj + width]] <- viewport(name = paste("yaxis", jj, sep="."),
##                                                     layout.pos.col = 1,
##                                                     layout.pos.row = jj)
##   }
  
  pushViewport(vpTree(top.vp, do.call(vpList, subplots)))
  for (ll in 1:num.facets) {
    seekViewport(paste(".subplot", ll, sep = "."))
    cat("Doing facet ")
    print(ll)
    facet.info <- get.facet.info(ll)
    .subplot(f, stats,
             plot.params = plot.params[[ll]],
             scales = scales,
             .subset = facet.info,
             draw.top.strip = facet.info$top.strip,
             draw.right.strip = facet.info$right.strip,
             draw.y.axis = facet.info$x == 1,
             draw.x.axis = facet.info$y == 1,
             allocate.y.axis.space = FALSE,
             allocate.x.axis.space = FALSE)
  }
  popViewport()
}


.collapse.scales <- function(color.expr,
                             fill.expr,
                             size.expr,
                             scales) {
  if (!is.null(color.expr)) {
  }

  if (!is.null(fill.expr)) {
  }

  if (!is.null(size.expr)) {
  }

  eval.color == eval.color
}

.draw.legend <- function(scale) {
  
}

### ENTRY POINT ###
jjplot <- function(f, data = NULL, color = NULL,
                   fill = NULL, size = NULL, alpha = 1.0,
                   log.x = FALSE, log.y = FALSE,
                   xlab = NULL, ylab = NULL,
                   facet.x = NULL, facet.y = NULL,
                   facet.nrow = NA, facet.ncol = NA,
                   expand = c(0.04, 0.04),
                   color.scale = NULL, fill.scale = NULL) {
  eval.color <- eval(match.call()$color, data)
  eval.fill <- eval(match.call()$fill, data)
  eval.size <- eval(match.call()$size, data)
  eval.facet.x <- eval(match.call()$facet.x, data)
  eval.facet.y <- eval(match.call()$facet.y, data)  
  
  scales <- list()
  scales$color <- .make.color.scale(eval.color, alpha, manual = color.scale)
  scales$fill <- .make.color.scale(eval.fill, alpha, manual = fill.scale)
  scales$size <- .make.size.scale(eval.size)    

  ## Compute stats.
  stats <- .formula.apply(f, .call.with.data, function(...) NULL, data,
                          facet.x = eval.facet.x,
                          facet.y = eval.facet.y,
                          color = eval.color,
                          fill = eval.fill,
                          size = eval.size)
  
  grid.newpage()
  if (is.null(eval.facet.x) && is.null(eval.facet.y)) {
    ## Compute plotting parameters.
    plot.params <- .get.plot.params(f, stats, log.x, log.y, expand,
                                    xlab = xlab, ylab = ylab)
    
    ## Do the plot.
    .subplot(f, stats, plot.params, scales)
  } else {
    .faceted.plot(f, stats,
                  eval.facet.x, eval.facet.y,
                  facet.nrow, facet.ncol,
                  scales,
                  log.x, log.y, expand,
                  xlab = xlab, ylab = ylab)
  }
}
