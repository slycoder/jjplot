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
           alpha = NULL,
           color = NULL,
           data = NULL,
           ...) {
    eval.x <- eval(match.call()$x, data)
    eval.y <- eval(match.call()$y, data)
    eval.alpha <- eval(match.call()$alpha, data)
    eval.color <- eval(match.call()$color, data)

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
    
    qplot.point <- function(pch = 16, col = NULL) {      
      points(layer.data$x, layer.data$y,
             pch = pch,
             col = match.colors(col, eval.color))
    }
    
    match.colors <- function(override.col, facet, use.alpha = TRUE) {
      if (!is.null(override.col)) {
        override.col
      } else if (is.null(facet)) {
        "black"
      } else if (use.alpha) {
        colors.with.alpha[facet]
      } else {
        colors[facet]        
      }
    }

    if (is.null(eval.color)) {
      colors <- "black"
    } else {
      stopifnot(is.factor(eval.color))
      colors <- hcl(h = seq(0, 360, length.out = nlevels(eval.color) + 1),
                    l = 75,
                    c = 55)
      colors <- colors[-length(colors)]
    }

    if (!is.null(eval.alpha)) {
      colors.with.alpha <- rgb(t(col2rgb(colors)),
                               alpha = eval.alpha * 255,
                               maxColorValue = 255)
    }

    is.stat <- function(layer.call) {
      return (as.character(layer.call[[1]]) %in% stats.list)
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
    } else {
      pretty.y <- pretty(yrange)
      labels.y <- TRUE      
    }
    
    plot.new()
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
      } else {
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
    axis(side=2, pretty.y, col = "grey50", col.axis = "grey50",
         mgp = c(3, 0.75, 0), las=1, cex.axis = .9, labels = labels.y)
    title(main = match.call(),
          xlab = match.call()$x, ylab = match.call()$y)
}


