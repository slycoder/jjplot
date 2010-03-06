#!Rscript

require("grid")

jjplot <-
  function(x,
           y = NULL,
           data,
           ...) {
    op <- par(no.readonly = TRUE)
    stats.list <- c("jjplot.fun.x",
                    "jjplot.fun.y",
                    "jjplot.fit",
                    "jjplot.jitter",
                    "jjplot.identity",
                    "jjplot.quantile",
                    "jjplot.table",
                    "jjplot.ccdf",
                    "jjplot.cumsum",
                    "jjplot.hist",
                    "jjplot.facet")
    geoms.list <- c("jjplot.hline",
                    "jjplot.vline",
                    "jjplot.abline",
                    "jjplot.point",
                    "jjplot.line",
                    "jjplot.text",
                    "jjplot.bar",
                    "jjplot.box")
    is.stat <- function(layer.call) {
      return(is.call(layer.call) && as.character(layer.call[[1]]) %in% stats.list)
    }

    is.geom <- function(layer.call) {
      return(is.call(layer.call) && as.character(layer.call[[1]]) %in% geoms.list)
    }

    # flag whether x or y axis is to be log-transformed
    log.arg = match.call()$log
    log.x <- (!is.null(log.arg) && (log.arg == 'x' || log.arg == 'xy'))
    log.y <- (!is.null(log.arg) && (log.arg == 'y' || log.arg == 'xy'))
    eval.x <- eval(match.call()[["x"]], data)
    ylab.default <- NULL
    
    if(log.x && is.numeric(eval.x)) {
      eval.x <- log10(eval.x)
    }

    if (is.stat(match.call()[["y"]]) || is.geom(match.call()[["y"]])) {
      eval.y <- NULL
    } else {
      eval.y <- eval(match.call()[["y"]], data)
      if(log.y && is.numeric(eval.y)) {
        eval.y <- log10(eval.y)
      }
    }


    eval.alpha <- eval(match.call()$alpha, data)
    eval.color <- eval(match.call()$color, data)
    eval.fill <- eval(match.call()$fill, data)
    eval.size <- eval(match.call()$size, data)

    eval.expand <- eval(match.call()$expand, data)    
    if (is.null(eval.expand))
      eval.expand <- c(0.04, 0.04)

    color.expr <- match.call()$color
    fill.expr <- match.call()$fill
    size.expr <- match.call()$size
    x.expr <- match.call()$x
    y.expr <- match.call()$y

    facet.data <- NULL
    layer.data <- NULL

    squash.unused <- if (is.null(match.call()$squash.unused)) FALSE else eval(match.call()$squash.unused)
    
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
      df <- data.frame(x = names(tt), y = as.numeric(tt))
      if(log.y) {
      	df$y <- log10(df$y)
      }
      ylab.default <<- substitute(Count(x),list(x=x.expr))
      df
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
      data.frame(x = facet.data$x[oo], y = cumsum(facet.data$y[oo]))
    }
    
    jjplot.identity <- function() {
      facet.data
    }

    jjplot.hist <- function(align = c("left", "right", "middle"),
                            breaks = 20,
                            ...) {
      align <- match.arg(align)
      h <- hist(facet.data$x, breaks = breaks, plot = FALSE, ...)
      if (align == "left") {
        hx <- h$breaks[-length(h$breaks)]
      } else if (align == "right") {
        hx <- h$breaks[-1]
      } else {
        hx <- h$mids
      }
      data.frame(x = hx, y = h$density)
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

      xstart <- (ylim[1] - layer.data$b) / layer.data$a
      xend <- (ylim[2] - layer.data$b) / layer.data$a      

#      ystart <- ifelse(xstart < xlim[1] | xstart > xlim[2],
      ystart <- ifelse(xstart < xlim[1],
                       ystart,
                       ylim[1])
#      yend <- ifelse(xend > xlim[2] | xend < xlim[1],
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
    
    jjplot.point <- function(pch = 16, col = NULL, size = NULL) {
      grid.points(layer.data$x,
                  layer.data$y,
                  pch = pch,
                  size = unit(0.5 * match.sizes(size, layer.data$size), "char"),
                  gp = gpar(alpha = eval.alpha,
                    col = match.colors(col, layer.data$color),
                    fill = match.colors(col, layer.data$fill, use.fill = TRUE)))
    }

    jjplot.line <- function(lty = "solid", col = NULL, lwd = 1.5, ordered = TRUE) {
      if (ordered) {
        oo <- order(layer.data$x)
      } else {
        oo <- 1:nrow(layer.data)
      }
      if (!is.null(layer.data$color) && is.null(col))  {
        by(layer.data[oo,],
           layer.data$color[oo],
           function(zz)
           grid.lines(x = zz$x, y = zz$y,
                      default.units = "native",
                      gp = gpar(col = match.colors(col, zz$color),
                        lwd = lwd,
                        lty = lty)))
      } else {
        grid.lines(x = layer.data$x[oo], y = layer.data$y[oo],
                   default.units = "native",
                   gp = gpar(col = match.colors(col, layer.data$color[oo]),
                     lwd = lwd,
                     lty = lty))
      }
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
    
    match.sizes <- function(override.size, facet) {
      if (!is.null(override.size)) {
        override.size
      } else if (is.null(facet)) {
        1.0
      } else {
        sizes(facet)
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
    

    if (is.null(eval.alpha)) {
      eval.alpha <- 1.0
    }
      
    colors <- make.color.scale(eval.color, 1.0)
    fills <- make.color.scale(eval.fill, 1.0)
    sizes <- make.size.scale(eval.size)    

    geom.expansion <- function(.call) {
      if (is.call(.call) && as.character(.call[[1]]) == "jjplot.bar") {
        list(y = 0)
      } else if (is.call(.call) && as.character(.call[[1]]) == "jjplot.box") {
        list(y = c(layer.data$quantile.0, layer.data$quantile.100))
      } else {
        list()
      }
    }
        
    eval.grid.x <- eval(match.call()$grid.x, data)
    eval.grid.y <- eval(match.call()$grid.y, data)
    stopifnot(is.null(eval.grid.x))

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
    
    nlayers <- length(match.call())
    layer.data.list <- list()
    for (layer in 1:nlayers) {
      layer.call <- match.call()[[layer]]
      
      facet.data <- data.frame(x = eval.x)
      if (!is.null(eval.y))
        facet.data$y <- eval.y
      if (!is.null(eval.color))
        facet.data$color <- eval.color
      if (!is.null(eval.fill))
        facet.data$fill <- eval.fill
      if (!is.null(eval.size))
        facet.data$size <- eval.size
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

        layer.data.list <- c(layer.data.list,
                             list(cur.layer))
      }
    }
    
    grid.newpage()


    .set.plotting.layer <- function(layer.data.index, .subset) {
      if (layer.data.index == 0) {
        ## Implicit identity
        layer.data <<- data.frame(x = eval.x)
        if (!is.null(eval.y)) {
          layer.data <<- cbind(layer.data, y = eval.y)
        }
        if (!is.null(eval.color)) {
          layer.data <<- cbind(layer.data, color = eval.color)
        }
        if (!is.null(eval.fill)) {
          layer.data <<- cbind(layer.data, fill = eval.fill)
        }
        if (!is.null(eval.size)) {
          layer.data <<- cbind(layer.data, size = eval.size)
        }
      } else {
        layer.data <<- layer.data.list[[layer.data.index]]
      }

      if (!is.null(.subset)) {
        layer.data <<- subset(layer.data, layer.data$.facet == .subset) 
      }

      NULL
    }
    
    .get.plot.range <- function(calls, .subset = NULL) {
      xrange <- NULL
      yrange <- NULL
      
      stat.before.geom <- FALSE
      layer.data.index <- 0
      first.y <- NULL
      .set.plotting.layer(layer.data.index, .subset)
      for (layer in 1:nlayers) {
        layer.call <- calls[[layer]]
        if (is.stat(layer.call)) {
          layer.data.index <- layer.data.index + 1
          ## Get the range of the data on the layer.
          .set.plotting.layer(layer.data.index, .subset)
          xrange <- expand.range(xrange, layer.data$x)
          yrange <- expand.range(yrange, layer.data$y)
          stat.before.geom <- TRUE
        } 
        if (!is.null(layer.data$y) && length(layer.data$y) > 0 && is.null(first.y)) {
          first.y <- layer.data$y
        }
        if (is.geom(layer.call)) {
          extra.data <- geom.expansion(layer.call)
          xrange <- expand.range(xrange, extra.data$x)
          yrange <- expand.range(yrange, extra.data$y)
        }
        if (is.geom(layer.call) && !stat.before.geom) {
          xrange <- expand.range(xrange, eval.x)
          yrange <- expand.range(yrange, eval.y)    
        }
      }
      
      .set.plotting.layer(0, .subset)
      if (is.factor(layer.data$x)) {    
        pretty.x <- 1:nlevels(layer.data$x)
        labels.x <- levels(layer.data$x)
        label.x.height <- convertHeight(unit(1, "strheight", labels.x[which.max(nchar(labels.x))]),
                                        "lines", valueOnly = TRUE) + 2.1
      } else {
        pretty.x <- pretty(xrange)
        labels.x <- TRUE
        label.x.height <- 3.1
        if (!is.null(calls$labels.x)) {
          labels.x <- eval(calls$labels.x, parent.frame(n = 4))
          label.x.height <- convertHeight(unit(1, "strheight", labels.x[which.max(nchar(labels.x))]),
                                          "lines", valueOnly = TRUE) + 2.1
        }
      }
      if (log.x) {
     	labels.x <- sapply(pretty.x, function(x)
                           substitute(10^x, list(x = x)),
                           simplify=FALSE)
        labels.x <- do.call(expression, labels.x)
      }
      if (is.factor(first.y)) {
        ## Figure how wide the text is going to be:
        labels.y <- levels(first.y)        
        if (squash.unused) {
          first.y <- factor(first.y)
          yrange <- c(1, nlevels(first.y))
        }
        pretty.y <- 1:(nlevels(first.y))
        labels.y <- levels(first.y)
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
      print(label.y.width)
        
      xrange <- range(c(xrange, pretty.x))    
      yrange <- range(c(yrange, pretty.y))
      
      xrange[1] <- xrange[1] - (xrange[2] - xrange[1]) * eval.expand[1]
      xrange[2] <- xrange[2] + (xrange[2] - xrange[1]) * eval.expand[1]
      
      yrange[1] <- yrange[1] - (yrange[2] - yrange[1]) * eval.expand[2]
      yrange[2] <- yrange[2] + (yrange[2] - yrange[1]) * eval.expand[2]

      list(xrange = xrange, yrange = yrange, pretty.x = pretty.x, pretty.y = pretty.y,
           labels.x = labels.x, labels.y  = labels.y, label.x.height = label.x.height, label.y.width = label.y.width)
    }
    
    
    ..subplot <- function(calls, plot.params,
                          .subset = NULL,
                          draw.x.axis = TRUE,
                          allocate.x.axis.space = TRUE) {

      if (draw.x.axis && allocate.x.axis.space) {
        xmargin <- plot.params$label.x.height
      } else {
        xmargin <- 0
      }

      if (!is.null(calls$main)) {
        titlemargin <- 2.1
      } else {
        titlemargin <- 0.4
      }
      
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

      layer.data.index <- 0
      for (layer in 1:nlayers) {
        layer.call <- calls[[layer]]
        if (is.stat(layer.call)) {
          layer.data.index <- layer.data.index + 1
        } else if (is.geom(layer.call)) {
          cat("Working on geom ")
          print(layer)
          .set.plotting.layer(layer.data.index, .subset)

          if (squash.unused && is.factor(layer.data$y)) {
            layer.data$y <<- factor(layer.data$y)
          }
          
          eval(layer.call)
        }
      }

      if (draw.x.axis) {
        grid.xaxis(at = plot.params$pretty.x,
                   label = plot.params$labels.x,
                   gp = gpar(col = "grey50", cex = 0.8))
      }
      grid.yaxis(at = plot.params$pretty.y,
                 label = plot.params$labels.y,
                 gp = gpar(col = "grey50", cex = 0.8))
      
      
      if (is.null(calls$ylab)) {
      	if(is.null(ylab.default)) {
          grid.text(calls$y, x = unit(-plot.params$label.y.width - 1.5, "lines"), rot = 90,
                    gp = gpar(col = "grey20", cex= 0.9))
        } else {
          grid.text(ylab.default, x = unit(-plot.params$label.y.width - 1.5, "lines"), rot = 90,
                    gp = gpar(col = "grey20", cex= 0.9))
        }        	      
      } else {
        grid.text(eval(calls$ylab), x = unit(-plot.params$label.y.width - 1.5, "lines"), rot = 90,
                  gp = gpar(col = "grey20", cex= 0.9))
      }
      if (draw.x.axis) {
        if (is.null(calls$xlab)) {
          grid.text(calls$x, y = unit(-plot.params$label.x.height + 0.5, "lines"),
                    gp = gpar(col = "grey20", cex= 0.9))
        } else {
          grid.text(eval(calls$xlab), y = unit(-plot.params$label.x.height + 0.5, "lines"),
                    gp = gpar(col = "grey20", cex= 0.9))
        }
      }
      if (!is.null(calls$main)) {
        grid.text(calls$main, y = unit(2, "lines"))
      }

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

