#!Rscript

grid.multi <- function(lty = "solid",
                       col = "white",
                       size.major = 1.5,
                       size.minor = 0.5) {
  x.ticks <- axTicks(1)
  y.ticks <- axTicks(2)
  abline(v = x.ticks,
         lty = lty, col = col, lwd = size.major)
  abline(h = y.ticks,
         lty = lty, col = col, lwd = size.major)

  abline(v = (x.ticks[-1] + x.ticks[-length(x.ticks)]) / 2,
         lty = lty, col = col, lwd = size.minor)         
  abline(h = (y.ticks[-1] + y.ticks[-length(y.ticks)]) / 2,
         lty = lty, col = col, lwd = size.minor)         
}

qplot.fast <-
  function(x = NULL,
           y = NULL,
           data = NULL,
           ...) {
    eval.x <- eval(match.call()$x, data)
    eval.y <- eval(match.call()$y, data)
    eval.alpha <- eval(match.call()$alpha, data)
    eval.color <- eval(match.call()$color, data)
    eval.fill <- eval(match.call()$fill, data)
    eval.size <- eval(match.call()$size, data)
    
    facet.x <- NULL
    facet.y <- NULL
    
    qplot.facet <- function(f, facet = NULL) {
      eval.facet <- eval(match.call()$facet, data)
      stopifnot(!is.null(eval.facet))

      facet.call <- match.call()$f
      
      faceted.df <- by(data.frame(x = eval.x, y = eval.y,
                                  f = eval.facet),
                       eval.facet,
                       function(df) { facet.x <<- df$x;
                                      facet.y <<- df$y;
                                      cbind(eval(facet.call),
                                            f = df$f[1]) })
      do.call(rbind, faceted.df)
    }

    ## Stats
    stats.list <- c("qplot.fun.x",
                    "qplot.fun.y",
                    "qplot.fit",
                    "qplot.jitter",
                    "qplot.identity",
                    "qplot.facet")
    
    qplot.fun.x <- function(f) {
      data.frame(x = f(facet.x))
    }

    qplot.fun.y <- function(f) {
      data.frame(y = f(facet.y))
    }

    qplot.fit <- function() {
      model <- lm(facet.y ~ facet.x)
      data.frame(b = coef(model)[1],
                 a = coef(model)[2])
    }

    qplot.jitter <- function(xfactor = 0, yfactor = 0) {
      data.frame(x = jitter(as.numeric(facet.x), xfactor),
                 y = jitter(as.numeric(facet.y), yfactor))
    }
    
    qplot.identity <- function() {
      data.frame(x = facet.x, y = facet.y)
    }

    ## Geoms
    geoms.list <- c("qplot.hline",
                    "qplot.vline",
                    "qplot.abline",
                    "qplot.point")
    
    qplot.hline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      abline(h = layer.data$y,
             col = match.colors(col, layer.data$f, use.alpha = F),
             lwd = lwd,
             lty = lty)
    }    
    
    qplot.vline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      abline(v = layer.data$x,
             col = match.colors(col, layer.data$f, use.alpha = F),
             lwd = lwd,
             lty = lty)
    }

    qplot.abline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      for (ii in 1:nrow(layer.data)) {
        abline(b = layer.data$a[ii],
               a = layer.data$b[ii],             
               col = match.colors(col, layer.data$f[ii], use.alpha = F),
               lwd = lwd,
               lty = lty)
      }
    }
    
    qplot.point <- function(pch = 16, col = NULL, size = 1.0) {
      points(layer.data$x, layer.data$y,
             pch = pch,
             cex = size,
             col = match.colors(col, eval.color),
             bg = match.colors(col, eval.fill, use.fill=TRUE))
    }
    
    match.colors <- function(override.col, facet,
                             use.fill = FALSE,
                             use.alpha = TRUE) {
      if (!is.null(override.col)) {
        override.col
      } else if (is.null(facet)) {
        "black"
      } else if (use.alpha) {
        if (use.fill) {
          fills.with.alpha(facet)
        } else {
          colors.with.alpha(facet)          
        }
      } else {
        if (use.fill) {
          fills(facet)
        } else {
          colors(facet)
        }
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

    if (is.null(eval.alpha)) {
      eval.alpha <- 1.0
    }
      
    colors <- make.color.scale(eval.color, 1.0)
    fills <- make.color.scale(eval.fill, 1.0)

    colors.with.alpha <- make.color.scale(eval.color, eval.alpha)
    fills.with.alpha <- make.color.scale(eval.fill, eval.alpha)

    if (is.null(eval.size)) {
      eval.size <- 1
    }
    
    is.stat <- function(layer.call) {
      return (as.character(layer.call[[1]]) %in% stats.list)
    }

    is.geom <- function(layer.call) {
      return (as.character(layer.call[[1]]) %in% geoms.list)
    }
    
    if (is.factor(eval.x)) {
      xrange <- range(1:nlevels(eval.x))
    } else {
      xrange <- range(eval.x)
    }

    if (is.factor(eval.y)) {
      yrange <- range(1:nlevels(eval.y))      
    } else {
      yrange <- range(eval.y)
    }

    nlayers <- length(match.call()) - 6
    layer.data.list <- list()
    for (layer in 1:nlayers) {
      layer.call <- match.call()[[layer + 6]]
      facet.x <- eval.x
      facet.y <- eval.y
      if (is.stat(layer.call)) {
        cat("Working on stat ")
        print(layer)
        cur.layer <- eval(layer.call)
        layer.data.list <- c(layer.data.list,
                             list(cur.layer))
        xrange <- range(c(xrange, cur.layer$x))
        yrange <- range(c(yrange, cur.layer$y))        
      }
    }
    
    xrange[1] <- xrange[1] - (xrange[2] - xrange[1]) * .04
    xrange[2] <- xrange[2] + (xrange[2] - xrange[1]) * .04

    if (is.factor(eval.x)) {    
      pretty.x <- 1:nlevels(eval.x)
      labels.x <- levels(eval.x)
    } else {
      pretty.x <- pretty(xrange)
      labels.x <- TRUE
    }

    yrange[1] <- yrange[1] - (yrange[2] - yrange[1]) * .04
    yrange[2] <- yrange[2] + (yrange[2] - yrange[1]) * .04

    if (is.factor(eval.y)) {    
      pretty.y <- 1:nlevels(eval.y)
      labels.y <- levels(eval.y)
      label.y.width <- max(nchar(levels(eval.y))) - 4
    } else {
      pretty.y <- pretty(yrange)
      labels.y <- TRUE
      label.y.width <- 4      
    }
    
    plot.new()
    par(mar = c(3, label.y.width, 1, 1))
    plot.window(xlim = xrange, ylim = yrange, xaxs = "i", yaxs = "i")
    rect(xrange[1], yrange[1],
         xrange[2], yrange[2],
         col = "grey90", border = NA)
    grid.multi()

    layer.data.index <- 0
    for (layer in 1:nlayers) {
      layer.call <- match.call()[[layer + 6]]
      if (is.stat(layer.call)) {
        layer.data.index <- layer.data.index + 1
      } else if (is.geom(layer.call)) {
        cat("Working on geom ")
        print(layer)
        if (layer.data.index == 0) {
          ## Implicit identity
          layer.data <- data.frame(x = eval.x,
                                   y = eval.y)
        } else {
          layer.data <- layer.data.list[[layer.data.index]]
        }
        eval(layer.call)
      }
    }

    axis(side=1, pretty.x, col = "grey50", col.axis = "grey50",
         mgp = c(3, 0.5, 0), cex.axis = .9, labels = labels.x)
    par(mgp = c(label.y.width - 1, 0, 0))
    axis(side=2, pretty.y, col = "grey50", col.axis = "grey50",
         mgp = c(3, 0.75, 0), las=1, cex.axis = .9, labels = labels.y)
    title(ylab = match.call()$y)
    par(mgp = c(1.5, 0, 0))
    title(xlab = match.call()$x)
}


