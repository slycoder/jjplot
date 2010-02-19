#!Rscript

require("grid")

jjplot <-
  function(x = NULL,
           y = NULL,
           data = NULL,
           ...) {
    op <- par(no.readonly = TRUE)
    
    eval.x <- eval(match.call()$x, data)
    eval.y <- eval(match.call()$y, data)
    eval.alpha <- eval(match.call()$alpha, data)
    eval.color <- eval(match.call()$color, data)
    eval.fill <- eval(match.call()$fill, data)
    eval.size <- eval(match.call()$size, data)

    color.expr <- match.call()$color
    fill.expr <- match.call()$fill   

    facet.data <- NULL
    
    jjplot.facet <- function(f, facet = NULL) {
      eval.facet <- eval(match.call()$facet, data)
      stopifnot(!is.null(eval.facet))

      facet.call <- match.call()$facet
      f.call <- match.call()$f
      
      faceted.df <- by(cbind(facet.data, .facet = eval.facet),
                       eval.facet,
                       function(df) { facet.data <<- df
                                      result <- eval(f.call)
                                      if (!is.null(fill.expr) && facet.call == fill.expr)
                                        result$fill <- df$.facet[1]
                                      if (!is.null(color.expr) && facet.call == color.expr)
                                        result$color <- df$.facet[1]
                                      result$.facet <- df$.facet[1]
                                      result
                                    })
      do.call(rbind, faceted.df)
    }

    ## Stats
    stats.list <- c("jjplot.fun.x",
                    "jjplot.fun.y",
                    "jjplot.fit",
                    "jjplot.jitter",
                    "jjplot.identity",
                    "jjplot.quantile",
                    "jjplot.table",
                    "jjplot.facet")
    
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

    jjplot.jitter <- function(xfactor = 0, yfactor = 0) {
      transform(facet.data,
                x = jitter(as.numeric(facet.data$x), xfactor),
                y = jitter(as.numeric(facet.data$y), yfactor))
    }
    
    jjplot.quantile <- function() {
      stopifnot(all(facet.data$x == facet.data$x[1]))
      result <- data.frame(facet.data$x[1], t(quantile(facet.data$y)))
      colnames(result) <- c("x", "quantile.0", "quantile.25", "quantile.50", "quantile.75", "quantile.100")
      rownames(result) <- NULL
      result
    }

    jjplot.table <- function() {
      tt <- table(facet.data$x)
      print(data.frame(x = names(tt), y = as.numeric(tt)))
    }    
    
    jjplot.identity <- function() {
      facet.data
    }

    ## Geoms
    geoms.list <- c("jjplot.hline",
                    "jjplot.vline",
                    "jjplot.abline",
                    "jjplot.point",
                    "jjplot.line",
                    "jjplot.text",
                    "jjplot.bar",
                    "jjplot.box")
    
    jjplot.hline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      grid.lines(y = layer.data$y,
                 default.units = "native",
                 gp = gpar(col = match.colors(col, layer.data$color),
                   lwd = lwd,
                   lty = lty))
    }    
    
    jjplot.vline <- function(lwd = 1.5, col = NULL, lty = "solid") {
      grid.lines(x = layer.data$x,
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

      xstart <- (ylim[1] - layer.data$b) / layer.data$a
      xend <- (ylim[2] - layer.data$b) / layer.data$a      

      ystart <- ifelse(xstart < xlim[1],
                       ystart,
                       ylim[1])
      yend <- ifelse(xend > xlim[2],
                     yend,
                     ylim[2])
      
      xstart <- pmax(xstart, xlim[1])
      xend <- pmin(xend, xlim[2])      
      
      grid.segments(xstart, ystart, xend, yend,
                    default.units = "native",
                    gp = gpar(lwd = lwd,
                      lty = lty,
                      col = match.colors(col, layer.data$color)))
    }
    
    jjplot.point <- function(pch = 16, col = NULL, size = 1.0) {
      grid.points(layer.data$x,
                  layer.data$y,
                  pch = pch,
                  size = unit(0.5 * size, "char"),
                  gp = gpar(alpha = eval.alpha,
                    col = match.colors(col, layer.data$color),
                    fill = match.colors(col, layer.data$fill, use.fill = TRUE)))
    }

    jjplot.line <- function(lty = "solid", col = NULL, lwd = 1.5) {
      grid.lines(x = layer.data$x, y = layer.data$y,
                 default.units = "native",
                 gp = gpar(col = match.colors(col, layer.data$color),
                   lwd = lwd,
                   lty = lty))
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
                gp = gpar(col = match.colors(col, layer.data$color)))
    }

    jjplot.bar <- function(col = NULL, fill = NULL, width = 1) {
      grid.rect(layer.data$x,
                0,
                width,
                layer.data$y,
                just = c("center", "bottom"),
                default.units = "native",
                gp = gpar(fill = match.colors(fill, layer.data$fill, use.fill = TRUE),                  
                  col = match.colors(col, layer.data$color)))
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

    
    match.colors <- function(override.col, facet,
                             use.fill = FALSE) {
      if (!is.null(override.col)) {
        override.col
      } else if (is.null(facet)) {
        "black"
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
      xrange <- c(0.5, nlevels(eval.x) + 0.5)
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

      facet.data <- data.frame(x = eval.x,
                               y = eval.y)
      if (!is.null(eval.color))
        facet.data$color <- eval.color
      if (!is.null(eval.fill))
        facet.data$fill <- eval.fill
      if (is.stat(layer.call)) {
        cat("Working on stat ")
        print(layer)
        if (!is.null(eval.grid.y)) {
          cur.layer <- eval(substitute(jjplot.facet(layer.call, grid.call),
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
    
#    xrange[1] <- xrange[1] - (xrange[2] - xrange[1]) * .04
#    xrange[2] <- xrange[2] + (xrange[2] - xrange[1]) * .04

    if (is.factor(eval.x)) {    
      pretty.x <- 1:nlevels(eval.x)
      labels.x <- levels(eval.x)
    } else {
      pretty.x <- pretty(xrange)
      labels.x <- TRUE
    }

#    yrange[1] <- yrange[1] - (yrange[2] - yrange[1]) * .04
#    yrange[2] <- yrange[2] + (yrange[2] - yrange[1]) * .04

    if (is.factor(eval.y)) {    
      pretty.y <- 1:nlevels(eval.y)
      labels.y <- levels(eval.y)
      label.y.width <- max(nchar(levels(eval.y))) - 4
    } else {
      pretty.y <- pretty(yrange)
      labels.y <- TRUE
      label.y.width <- 4      
    }

    xrange <- range(c(xrange, pretty.x))    
    yrange <- range(c(yrange, pretty.y))
    
    grid.newpage()
    ..subplot <- function(calls, .subset = NULL,
                          draw.x.axis = TRUE) {
      grid.rect()
      pushViewport(plotViewport(c(3.1, 4.1, 2.1, 1.1)))      
      pushViewport(dataViewport(xscale = xrange,
                                yscale = yrange))
      grid.rect(gp = gpar(fill = "grey90", col = "white"))
      grid.grill(pretty.y, pretty.x,
                 gp = gpar(col = "white", lwd = 1.5),
                 default.units = "native")

      midpoints <- function(v) {
        (v[-1] + v[-length(v)]) / 2
      }
      
      grid.grill(midpoints(pretty.y), midpoints(pretty.x),
                 gp = gpar(col = "white", lwd = 0.5),
                 default.units = "native")

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
        grid.xaxis(at = pretty.x,
                   label = labels.x,
                   gp = gpar(col = "grey50", cex = 0.8))
      }
      grid.yaxis(at = pretty.y,
                 label = labels.y,
                 gp = gpar(col = "grey50", cex = 0.8))
      
      if (is.null(calls$ylab)) {
        grid.text(calls$y, x = unit(-3, "lines"), rot = 90,
                  gp = gpar(col = "grey50", cex= 0.9))        
      } else {
        grid.text(calls$ylab, x = unit(-3, "lines"), rot = 90,
                  gp = gpar(col = "grey50", cex= 0.9))
      }
      if (draw.x.axis) {
        if (is.null(calls$xlab)) {
          grid.text(calls$x, y = unit(-2, "lines"),
                    gp = gpar(col = "grey50", cex= 0.9))
        } else {
          grid.text(calls$xlab, y = unit(-2, "lines"),
                    gp = gpar(col = "grey50", cex= 0.9))
        }
      }
      if (!is.null(.subset)) {
        grid.text(.subset, vjust = 0.0, y = unit(1, "npc"))
      } else if (!is.null(calls$main)) {
        grid.text(calls$main, y = unit(2, "lines"))
      }
      popViewport(2)
    }

    if (is.null(eval.grid.y)) {
      ..subplot(match.call())
    } else {
##       total.height <- par("din")[2] * 2.54
##       axis.padding <- 2 * par("mai")[1] / par("mar")[1] * 2.54
##       height <- total.height / (nlevels(eval.grid.y) + axis.padding)
##       height <- c(rep(height, nlevels(eval.grid.y) - 1),
##                   height + axis.padding)
      top.vp <- viewport(layout = grid.layout(nlevels(eval.grid.y), 1))
      subplots <- list()
      for (ll in 1:nlevels(eval.grid.y)) {
        subplots[[ll]] <- viewport(name = paste(".subplot", ll, sep = "."),
                                   layout.pos.row = ll, layout.pos.col = 1)
      }
      pushViewport(vpTree(top.vp, do.call(vpList, subplots)))
      for (ll in 1:nlevels(eval.grid.y)) {
        seekViewport(paste(".subplot", ll, sep = "."))
        cat("Doing facet ")
        print(ll)
        ..subplot(match.call(), .subset = levels(eval.grid.y)[ll],
                  draw.x.axis = T)
#                  draw.x.axis = ll == nlevels(eval.grid.y))
      }
      popViewport()
    }
}

