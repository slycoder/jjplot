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
                        x.expr, y.expr, scales,
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

jjplot.bar <- function(data, x.expr, y.expr, scales,
                       col = NULL, fill = NULL, width = 1) {
  grid.rect(data$x,
            0,
            width,
            data$y,
            just = c("center", "bottom"),
            default.units = "native",
            gp = gpar(fill = match.scale(fill, data$fill, scales, type = "fill"), 
              col = match.scale(col, data$color, scales)))
}

jjplot.point <- function(data, x.expr, y.expr, scales,
                         alpha = 1.0,
                         pch = 16, col = NULL, size = NULL) {
  grid.points(data$x,
              data$y,
              pch = pch,
              size = unit(0.5 * match.scale(size, data$size, scales, type="size"), "char"),
              gp = gpar(alpha = alpha,
                col = match.scale(col, data$color, scales),
                fill = match.scale(col, data$fill, scales, type="fill")))
}

jjplot.abline <- function(data, x.expr, y.expr, scales,
                          lwd = 1.5, col = NULL, lty = "solid") {
  ## Find limits
  xlim <- convertX(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)
  ylim <- convertY(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)      
  
  ystart <- xlim[1] * data$a + data$b
  yend <- xlim[2] * data$a + data$b
  
  xstart <- ifelse(data$a < 0,
                   (ylim[2] - data$b) / data$a,
                   (ylim[1] - data$b) / data$a)
  xend <- ifelse(data$a < 0,
                 (ylim[1] - data$b) / data$a,
                 (ylim[2] - data$b) / data$a)
  
  ## Invariant: xstart <= xend,
  ## So that left hand coordinate should be xtart or xlim[1]
  ## and right hand should be xend or xlim[2].      
  
  ystart <- ifelse(xstart < xlim[1],
                   ystart,
                   ifelse(data$a < 0,
                          ylim[2],
                          ylim[1]))
  
  yend <- ifelse(xend > xlim[2],
                 yend,
                 ifelse(data$a < 0,
                        ylim[1],
                        ylim[2]))
  
  xstart <- pmax(xstart, xlim[1])
  xend <- pmin(xend, xlim[2])
  
  grid.segments(xstart, ystart, xend, yend,
                default.units = "native",
                gp = gpar(lwd = lwd,
                  lty = lty,
                  col = match.scale(col, data$color, scales)))
}
    
jjplot.hline <- function(data, x.expr, y.expr, scales,
                         lwd = 1.5, col = NULL, lty = "solid") {
  grid.lines(y = data$y,
             default.units = "native",
             gp = gpar(col = match.scale(col, data$color, scales),
               lwd = lwd,
               lty = lty))
}    

jjplot.vline <- function(data, x.expr, y.expr, scales,
                         manual.x = NULL, lwd = 1.5, col = NULL, lty = "solid") {
  grid.lines(x = if (is.null(manual.x)) data$x else manual.x,
             default.units = "native",
             gp = gpar(col = match.scale(col, data$color, scales),
               lwd = lwd,
               lty = lty))
}

jjplot.text <- function(data, x.expr, y.expr, scales,
                        col = NULL, label = NULL,
                        x = NULL, y = NULL, hjust = 0.5,
                        vjust = 0.5) {
  if (is.null(x)) {
    x <- data$x
  }
  if (is.null(y)) {
    y <- data$y
  }
  if (is.null(label)) {
    label <- data$label
  }
  grid.text(label = label, x = x, y = y, 
            hjust = hjust, vjust = vjust,
                default.unit = "native",
            gp = gpar(col = match.scale(col, data$color, scales)))
}

jjplot.box <- function(data, x.expr, y.expr, scales,
                       col = NULL, fill = NULL, width = 0.5,
                       lwd = 1.5, lty = "solid") {
  grid.rect(as.numeric(data$x),
            data$quantile.25,
            width,
            data$quantile.75 - data$quantile.25,
            default.units = "native",
            just = c("center", "bottom"),
            gp = gpar(lwd = lwd,
              lty = lty,
              fill = match.scale(fill, data$fill, scales, type="fill"),
              col = match.scale(col, data$color, scales)))
  
  grid.segments(c(data$x, data$x, as.numeric(data$x) - width / 2),
                c(data$quantile.0, data$quantile.100, data$quantile.50),
                c(data$x, data$x, as.numeric(data$x) + width / 2),
                c(data$quantile.25, data$quantile.75, data$quantile.50),
                default.units = "native",
                gp = gpar(lwd = lwd,
                      lty = lty,
                  col = match.scale(col, data$color, scales)))
}

