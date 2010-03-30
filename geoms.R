### GEOMS ###
.is.geom <- function(layer.call) {
  return(is.call(layer.call) && exists(eval(paste('jjplot.geom',as.character(layer.call[[1]]),sep='.'))))
}

jjplot.geom.line <- function(state,
                             lty = "solid",
                             color = NULL,
                             lwd = 1.5, ordered = TRUE) {
  if (ordered) {
    oo <- order(state$data$x)
  } else {
    oo <- 1:nrow(state$data)
  }
  if (!is.null(state$data$color) && is.null(color))  {
    by(state$data[oo,], state$data$color[oo],
       function(zz)
       grid.lines(x = zz$x, y = zz$y,
                  default.units = "native",
                  gp = gpar(col = .match.scale(color, zz$color, state$scales),
                    lwd = lwd,
                    lty = lty)))
  } else {
    grid.lines(x = state$data$x[oo], y = state$data$y[oo],
               default.units = "native",
               gp = gpar(col = .match.scale(color, state$data$color[oo], state$scales),
                 lwd = lwd,
                 lty = lty))
  }
}

jjplot.geom.bar <- function(state,
                            color = NULL,
                            border = NULL, width = 1) {
  grid.rect(state$data$x,
            0,
            width,
            state$data$y,
            just = c("center", "bottom"),
            default.units = "native",
            gp = gpar(fill = .match.scale(color, state$data$color, state$scales), 
              col = .match.scale(border, state$data$border, state$scales, type = "border")))
}

jjplot.geom.fill <- function(data, x.expr, y.expr,
                         scales, col = NULL, fill = NULL) {
  grid.rect(data$x,
            data$y,
            1,
            1,
            just = c("center", "center"),
            default.units = "native",
            gp = gpar(fill = .match.scale(fill, data$fill, scales, type = "fill"), 
              col = .match.scale(col, data$color, scales)))         
}


jjplot.geom.area <- function(state,
                             color = NULL,
                             border = NULL) {
  if (is.null(state$data$color) || !is.null(color)) {
    grid.polygon(c(state$data$x[1], state$data$x, state$data$x[length(state$data$x)]),
                 c(0, state$data$y, 0),
                 default.units = "native",
                 gp = gpar(fill = .match.scale(color, state$data$color, scales), 
                   col = .match.scale(border, state$data$border, scales, type = "border")))
  } else {
    by(state$data, state$data$color, function(xx)
       grid.polygon(c(xx$x[1], xx$x, xx$x[length(xx$x)]),
                 c(0, xx$y, 0),
                 default.units = "native",
                 gp = gpar(fill = .match.scale(color, xx$color, state$scales), 
                   col = .match.scale(border, xx$border, state$scales, type = "border"))))
  }
}
  
jjplot.geom.point <- function(state,
                              alpha = 1.0,
                              pch = 16,
                              color = NULL,
                              border = NULL,
                              size = NULL) {
  grid.points(state$data$x,
              state$data$y,
              pch = pch,
              size = unit(0.5 * .match.scale(size, state$data$size, state$scales, type="size"), "char"),
              gp = gpar(alpha = alpha,
                col = .match.scale(color, state$data$color, state$scales)))
##                fill = .match.scale(border, state$data$border, state$scales, type="border")))
}

jjplot.geom.abline <- function(state,
                               lwd = 1.5, col = NULL, lty = "solid") {
  ## Find limits
  xlim <- convertX(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)
  ylim <- convertY(unit(c(0, 1), "npc"), "native", valueOnly = TRUE)      
  
  ystart <- xlim[1] * state$data$a + state$data$b
  yend <- xlim[2] * state$data$a + state$data$b
  
  xstart <- ifelse(state$data$a < 0,
                   (ylim[2] - state$data$b) / state$data$a,
                   (ylim[1] - state$data$b) / state$data$a)
  xend <- ifelse(state$data$a < 0,
                 (ylim[1] - state$data$b) / state$data$a,
                 (ylim[2] - state$data$b) / state$data$a)
  
  ## Invariant: xstart <= xend,
  ## So that left hand coordinate should be xtart or xlim[1]
  ## and right hand should be xend or xlim[2].      
  
  ystart <- ifelse(xstart < xlim[1],
                   ystart,
                   ifelse(state$data$a < 0,
                          ylim[2],
                          ylim[1]))
  
  yend <- ifelse(xend > xlim[2],
                 yend,
                 ifelse(state$data$a < 0,
                        ylim[1],
                        ylim[2]))
  
  xstart <- pmax(xstart, xlim[1])
  xend <- pmin(xend, xlim[2])
  
  grid.segments(xstart, ystart, xend, yend,
                default.units = "native",
                gp = gpar(lwd = lwd,
                  lty = lty,
                  col = .match.scale(col, state$data$color, state$scales)))
}
    
jjplot.geom.hline <- function(data, x.expr, y.expr, scales,
                          manual.y = NULL,
                          lwd = 1.5, col = NULL, lty = "solid") {
  grid.lines(y = if (is.null(manual.y)) data$y else manual.y,
             default.units = "native",
             gp = gpar(col = .match.scale(col, data$color, scales),
               lwd = lwd,
               lty = lty))
}    

jjplot.geom.vline <- function(data, x.expr, y.expr, scales,
                          manual.x = NULL, lwd = 1.5, col = NULL, lty = "solid") {
  grid.lines(x = if (is.null(manual.x)) data$x else manual.x,
             default.units = "native",
             gp = gpar(col = .match.scale(col, data$color, scales),
               lwd = lwd,
               lty = lty))
}

jjplot.geom.text <- function(data, x.expr, y.expr, scales,
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
            gp = gpar(col = .match.scale(col, data$color, scales)))
}


jjplot.geom.box <- function(data, x.expr, y.expr, scales,
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
              fill = .match.scale(fill, data$fill, scales, type="fill"),
              col = .match.scale(col, data$color, scales)))
  
  grid.segments(c(data$x, data$x, as.numeric(data$x) - width / 2),
                c(data$quantile.0, data$quantile.100, data$quantile.50),
                c(data$x, data$x, as.numeric(data$x) + width / 2),
                c(data$quantile.25, data$quantile.75, data$quantile.50),
                default.units = "native",
                gp = gpar(lwd = lwd,
                      lty = lty,
                  col = .match.scale(col, data$color, scales)))
}

.jjplot.expand.bar <- function(state,
                               width = 1.0,
                               color = NULL) {
  ## FIXME
  ##  xlim <- range(as.numeric(layer.data$x))
  ##  x.padding <- (xlim[2] - xlim[1]) * eval.expand[1]
  ##  if (x.padding >= width / 2) {
  ##    list(x = c(xlim[1] - width / 2 + x.padding,
  ##           xlim[2] + width / 2 - x.padding), y = 0)
  ##  } else {
  list(y = 0)
  ##  }
}

.jjplot.expand.box <- function(data, x.expr, y.expr, width = 0.5) {
  list(x = c(min(as.numeric(data$x)) - width / 2, max(as.numeric(data$x)) + width / 2),
       y = c(data$quantile.0, data$quantile.100))
}        

.jjplot.expand.area <- function(state) {
  list(y = 0)
}

.jjplot.expand.fill <- function(data, x.expr, y.expr) {
  list(y = c(0.5, nlevels(data$y) + 0.5),
       x = c(0.5, nlevels(data$x) + 0.5))       
}

