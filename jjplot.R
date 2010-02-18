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

    color.expr <- match.call()$color
    fill.expr <- match.call()$fill   
    
    facet.x <- NULL
    facet.y <- NULL
    facet.color <- NULL
    facet.fill <- NULL
    
    qplot.facet <- function(f, facet = NULL) {
      eval.facet <- eval(match.call()$facet, data)
      stopifnot(!is.null(eval.facet))

      facet.call <- match.call()$facet
      f.call <- match.call()$f
      
      faceted.df <- by(data.frame(x = eval.x, y = eval.y,
                                  f = eval.facet),
                       eval.facet,
                       function(df) { facet.x <<- df$x;
                                      facet.y <<- df$y;
                                      result <- eval(f.call)
                                      if (!is.null(fill.expr) && facet.call == fill.expr)
                                        result <- cbind(result, fill = df$f[1])
                                      if (!is.null(color.expr) && facet.call == color.expr)
                                        result <- cbind(result, color = df$f[1])
                                      result <- cbind(result, .facet = df$f[1])
                                    })
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
      result <- data.frame(x = facet.x, y = facet.y)
      if (!is.null(facet.color)) {
        result <- cbind(result, color = facet.color)
      }
      if (!is.null(facet.fill)) {
        result <- cbind(result, fill = facet.fill)
      }
      result
    }

    ## Geoms
    geoms.list <- c("qplot.hline",
                    "qplot.vline",
                    "qplot.abline",
                    "qplot.point",
                    "qplot.line",
                    "qplot.text")
    
    qplot.hline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      abline(h = layer.data$y,
             col = match.colors(col, layer.data$color,
               use.alpha = F),
             lwd = lwd,
             lty = lty)
    }    
    
    qplot.vline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      abline(v = layer.data$x,
             col = match.colors(col, layer.data$color,
               use.alpha = F),
             lwd = lwd,
             lty = lty)
    }

    qplot.abline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      for (ii in 1:nrow(layer.data)) {
        abline(b = layer.data$a[ii],
               a = layer.data$b[ii],             
               col = match.colors(col, layer.data$color[ii],
                 use.alpha = F),
               lwd = lwd,
               lty = lty)
      }
    }
    
    qplot.point <- function(pch = 16, col = NULL, size = 1.0) {
      points(layer.data$x, layer.data$y,
             pch = pch,
             cex = size,
             col = match.colors(col, layer.data$color),
             bg = match.colors(col, layer.data$fill, use.fill=TRUE))
    }

    qplot.line <- function(lty = "solid", col = NULL, lwd = 1.5) {
      lines(layer.data$x, layer.data$y,
             lty = lty,
             lwd = lwd,
             col = match.colors(col, layer.data$color))
    }

    qplot.text <- function(col = NULL, label = NULL,
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
      text(x, y = y, labels = label,
           adj = c(hjust, vjust),
           col = match.colors(col, layer.data$color))
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
      return(is.call(layer.call) && as.character(layer.call[[1]]) %in% stats.list)
    }

    is.geom <- function(layer.call) {
      return(is.call(layer.call) && as.character(layer.call[[1]]) %in% geoms.list)
    }

    eval.grid.x <- eval(match.call()$grid.x, data)
    eval.grid.y <- eval(match.call()$grid.y, data)
    stopifnot(is.null(eval.grid.x))
    
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

    nlayers <- length(match.call()) - 4
    layer.data.list <- list()
    for (layer in 1:nlayers) {
      layer.call <- match.call()[[layer + 4]]
      facet.x <- eval.x
      facet.y <- eval.y
      if (is.stat(layer.call)) {
        cat("Working on stat ")
        print(layer)
        if (!is.null(eval.grid.y)) {
          cur.layer <- eval(substitute(qplot.facet(layer.call, grid.call),
                                       list(layer.call = layer.call,
                                            grid.call = match.call()$grid.y)))
        } else {
          cur.layer <- eval(layer.call)
        }
        xrange <- range(c(xrange, cur.layer$x))
        yrange <- range(c(yrange, cur.layer$y))        
        layer.data.list <- c(layer.data.list,
                             list(cur.layer))
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

    ..subplot <- function(calls, .subset = NULL,
                          draw.x.axis = TRUE) {
      plot.new()      
      par(mar = c(ifelse(draw.x.axis, 3, 1), label.y.width, 1, 1))
      plot.window(xlim = xrange, ylim = yrange, xaxs = "i", yaxs = "i")
      rect(xrange[1], yrange[1],
           xrange[2], yrange[2],
           col = "grey90", border = NA)
      grid.multi()
      
      layer.data.index <- 0
      for (layer in 1:nlayers) {
        layer.call <- calls[[layer + 4]]
        if (is.stat(layer.call)) {
          layer.data.index <- layer.data.index + 1
        } else if (is.geom(layer.call)) {
          cat("Working on geom ")
          print(layer)
          if (layer.data.index == 0) {
            ## Implicit identity
            layer.data <<- data.frame(x = eval.x,
                                      y = eval.y)
            if (!is.null(eval.color)) {
              layer.data <<- cbind(layer.data, color = eval.color)
            }
            if (!is.null(eval.fill)) {
              layer.data <<- cbind(layer.data, fill = eval.fill)
            }
          } else {
            layer.data <<- layer.data.list[[layer.data.index]]
          }
          if (!is.null(.subset)) {
            layer.data <<- subset(layer.data, .facet == .subset) 
          }
          eval(layer.call)
        }
      }

      if (draw.x.axis) {
        axis(side=1, pretty.x, col = "grey50", col.axis = "grey50",
             cex.axis = .9, labels = labels.x)
      }
      par(mgp = c(label.y.width - 1, 1.0, 0))
      axis(side=2, pretty.y, col = "grey50", col.axis = "grey50",
           las=1, cex.axis = .9, labels = labels.y)
      if (is.null(calls$ylab)) {
        title(ylab = calls$y)
      } else {
        title(ylab = calls$ylab)
      }
      if (draw.x.axis) {
        par(mgp = c(1.5, 0.5, 0))      
        if (is.null(calls$xlab)) {
          title(xlab = calls$x)
        } else {
          title(xlab = calls$xlab)
        }
      }
      if (!is.null(.subset)) {
        title(main = .subset, cex.main = 1.0)        
      } else if (!is.null(calls$main)) {
        title(main = calls$main)
      }
    }

    if (is.null(eval.grid.y)) {
      ..subplot(match.call())
    } else {
      total.height <- par("din")[2] * 2.54
      axis.padding <- 2 * par("mai")[1] / par("mar")[1] * 2.54
      height <- total.height / (nlevels(eval.grid.y) + axis.padding)
      height <- c(rep(height, nlevels(eval.grid.y) - 1),
                  height + axis.padding)
      layout(matrix(1:nlevels(eval.grid.y),
                    nrow = nlevels(eval.grid.y)),
             height = lcm(height))
      for (ll in levels(eval.grid.y)) {
        cat("Doing facet ")
        print(ll)
        ..subplot(match.call(), .subset = ll,
                  draw.x.axis = ll == levels(eval.grid.y)[nlevels(eval.grid.y)])
      }
    }
}

## Code below should not be called by anyone!
## It sucks.

draw.strip <- function(x.left, x.right, y.top, txt,
                       flip = FALSE) {  
  height <- strheight(txt, units = "user", cex = 0.75) * 1.7
  if (!flip) {
    rect(x.left, y.top - height, x.right, y.top,
         col = "grey50", border = "black")
    text(x = (x.left + x.right) / 2,
         ## Makes vertical alignment more pleasant
         y = y.top - height / 2 - height * .05,
         labels = txt,
         cex = 0.75)
  } else {
    usr <- par("usr")
    rescale <- (usr[4] - usr[3]) / (usr[2] - usr[1])
    height <- height / rescale
    rect(y.top - height, x.right, y.top, x.left,
         col = "grey50", border = "black")
    
    text(y = ((x.left + x.right) / 2),
         x = (y.top - height / 2 - height * .05),
         labels = txt,
         srt = -90,
         cex = 0.75)
  }
}

split.plot <- function(splits, txts,
                       margin,
                       flip = FALSE) {
  usr <- par("usr")

  if (flip) {
    top <- usr[2]
    left <- usr[3]
    right <- usr[4]
  } else {
    top <- usr[4]
    left <- usr[1]
    right <- usr[2]
  }

  lefts <- c(left, splits + margin)
  rights <- c(splits - margin, right)

  stopifnot(length(lefts) == length(txts))

  for (ii in 1:length(splits)) {
    if (flip) {
      rect(usr[1], splits[ii] + margin,
           usr[2], splits[ii] - margin,
           col = "white", border = "white")
    } else {
      rect(splits[ii] - margin, usr[3], 
           splits[ii] + margin, usr[4], 
           col = "white", border = "white")
    }
  }

  for (ii in 1:length(lefts)) {
    draw.strip(lefts[ii], rights[ii],
               top, txts[ii], flip)

  }
}

