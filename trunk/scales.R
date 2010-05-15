### SCALES ###
.match.scale <- function(override, group, scales,
                         defaults = list(color = "black",
                           size = 1.0,
                           border = "black",
                           alpha = 1.0,
                           shape = 16),
                         type = c("color", "size", "border", "shape", "alpha")) {
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

jjplot.scale <- function(data, scale.params) {
  UseMethod("jjplot.scale")
}

jjplot.scale.default <- function(data, scale.params) {
  pp <- pretty(range(data))
  list(pretty = pp,
       labels = prettyNum(pp))
}

jjplot.scale.factor <- function(data, scale.params) {
  list(pretty = 1:nlevels(data),
       labels = levels(data))
}

jjplot.scale.Date <- function(data, scale.params) {
  ## FIXME:
  ## * Allow for user-specified formats
  ## * Allow for better pretty (eg, even dates)
  pp <- pretty(range(data))
  z <- as.Date(pp, origin="1970/01/01")
  list(pretty = pp,
       labels = format(z, format = "%Y/%m/%d"))
}

jjplot.scale.POSIXct <- function(data, scale.params) {
  ## See FIXME for jjplot.scale.Date.
  pp <- pretty(range(data))
  list(pretty = pp,
       labels = format(as.POSIXct(pp, origin="1970-01-01")))
}

.make.color.scale <- function(cc, alpha, manual = NULL) {
  if (is.null(cc) || length(cc) <= 1) {
    function(z) { rgb(0, 0, 0, alpha) }
  } else if (is.factor(cc)) {
    colors <- hcl(h = seq(0, 360, length.out = nlevels(cc) + 1),
                  l = 75,
                  c = 55,
                  alpha = alpha)
    colors <- colors[-length(colors)]
    function(z) { colors[z] }
  } else {    
    if (is.null(manual)) {
      if (range(cc)[1] >= 0) {
        cr <- colorRamp(c('white', 'blue'))
      } else {
        cr <- colorRamp(c('red', 'white', 'blue'))        
      }
    } else {
      cr <- manual
    }
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

.make.shape.scale <- function(cc) {
  if (is.null(cc) || length(cc) <= 1) {
    function(z) { 16 }
  } else if (is.factor(cc)) {
    shapes <- c(16, 15, 17, 18, 1, 0, 2, 3, 4, 5, 6)
    if (nlevels(cc) > length(shapes)) {
      stop("Too many levels for shape scale.")
    }
    function(z) { shapes[z] }
  } else {    
    stop("Shape scales can only be used with factors!")
  }
}

.make.size.scale <- function(ss) {
  if (is.null(ss)) {
    function(z) { 1.0 }
  } else {
    min.size <- 0.8
    max.size <- 3.3
    num.sizes <- 20
    size.levels <- seq(sqrt(min.size), sqrt(max.size), length.out = num.sizes)^2
    rr <- range(ss)
    function(z) { size.levels[round((num.sizes - 1) * (z - rr[1]) / (rr[2] - rr[1])) + 1] }
  }          
}

.make.alpha.scale <- function(ss) {
  if (is.null(ss)) {
    function(z) { 1.0 }
  } else {
    num.alphas <- 10
    size.levels <- sqrt(seq(0, 1, length.out = num.alphas))
    rr <- range(ss)
    function(z) { size.levels[round((num.alphas - 1) * (z - rr[1]) / (rr[2] - rr[1])) + 1] }
  }          
}

jjplot.theme.facet.x <- function(...) {
  themes <- list(...)
  function (x) {
    themes[[x$facet.x]](...)
  }
}

jjplot.theme.facet.y <- function(...) {
  themes <- list(...)
  function (x) {
    themes[[x$facet.y]](...)
  }
}

jjplot.theme.top.strip <- function(base.theme,
                                   .levels) {
  color.scale <- .make.color.scale(as.factor(.levels), 1.0)
  function(.subset) {
    theme <- base.theme(.subset)
    theme$top.strip.color <-
      color.scale(which(.levels == .subset$facet.x))
    theme
  }
}

jjplot.theme.right.strip <- function(base.theme,
                                     .levels) {
  color.scale <- .make.color.scale(as.factor(.levels), 1.0)
  function(.subset) {
    theme <- base.theme(.subset)
    theme$right.strip.color <-
      color.scale(which(.levels == .subset$facet.y))
    theme
  }
}

jjplot.theme <- function(theme = c("grey", "bw"),
                         ...) {
  themes <-
    list(grey = list(grid.color = "white",
           plot.background = "grey90",
           plot.border = "white",
           x.axis.color = "grey50",
           y.axis.color = "grey50",
           x.axis.title.color = "grey20",
           y.axis.title.color = "grey20",
           right.strip.color = "grey70",
           top.strip.color = "grey70",
           x.axis.angle = 0,
           y.axis.angle = 0),
         bw = list(grid.color = "grey90",
           plot.background = "white",
           plot.border = "black",
           x.axis.color = "black",
           y.axis.color = "black",
           x.axis.title.color = "black",
           y.axis.title.color = "black",
           right.strip.color = "grey70",
           top.strip.color = "grey70",
           x.axis.angle = 0,
           y.axis.angle = 0))
  theme <- match.arg(theme)
  theme <- themes[[theme]]
  theme[names(list(...))] <- list(...)
  function(...) {
    theme
  }
}
