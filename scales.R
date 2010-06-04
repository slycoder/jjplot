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

## Expands a range to incorporate new.data.
.jjplot.expand.range <- function(new.data, old.data) {
  UseMethod(".jjplot.expand.range")
}

.jjplot.expand.range.default <- function(new.data, old.data) {
  if (is.null(old.data) && is.null(new.data)) {
    return(NULL)
  } else {
    range(c(old.data, new.data))
  }
}

.jjplot.expand.range.factor <- function(new.data, old.data) {
  if (is.null(old.data)) {
    factor(levels(new.data))
  } else {
    stopifnot(levels(new.data) == levels(old.data))
    old.data
  }
}

.jjplot.expand.range.Date <- function(new.data, old.data) {
  range(c(new.data, old.data))
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
  stopifnot(!is.null(scale.params$factor.order))
  list(pretty = 1:length(scale.params$factor.order),
       labels = scale.params$factor.order)
}

jjplot.scale.Date <- function(data, scale.params) {
  ## FIXME:
  ## * Allow for user-specified formats
  pp <- .pretty.POSIXlt(data)
  list(pretty = as.numeric(pp),
       labels = format(pp, format = "%Y/%m/%d"))
}

## print(foo <- .pretty.POSIXlt(as.Date(c("2007-01-05", "2010-05-12"))))
## print(foo <- .pretty.POSIXlt(as.Date(c("2007-01-05", "2007-05-12"))))

.diffdate <- function(a, b, units = c("days", "weeks", "hours", "mins", "secs",
                              "years", "months")) {
  units <- match.arg(units)
  if (units == "years") {
    structure((as.POSIXlt(a)$year - as.POSIXlt(b)$year) + 
              (as.POSIXlt(a)$mon - as.POSIXlt(b)$mon) / 12,
              units = "years")
  } else if (units == "months") {
    structure((as.POSIXlt(a)$year - as.POSIXlt(b)$year) * 12 +
              (as.POSIXlt(a)$mon - as.POSIXlt(b)$mon),
              units = "months")
  } else {
    difftime(a, b, units = units)
  }  
}

.seq.date <- function(from, to, by = 1,
                      units = c("weeks", "days", "hours",
                        "mins", "secs", "years", "months")) {
  units <- match.arg(units)
  if (units == "months") {
    from.mon <- as.POSIXlt(from)$mon
    from.year <- as.POSIXlt(from)$year
    to.mon <- as.POSIXlt(to)$mon
    to.year <- as.POSIXlt(to)$year
    from.mon <- from.mon + from.year * 12
    to.mon <- to.mon + to.year * 12
    if (as.POSIXlt(to)$mday > 1) {
      to.mon <- to.mon + 1
    }
    months <- seq(from.mon, to.mon, by = by)    
    as.Date(paste(months %/% 12 + 1900, (months %% 12) + 1, "01", sep = "-"))                  
  } else if (units == "years") {
    from.year <- as.POSIXlt(from)$year
    to.year <- as.POSIXlt(to)$year
    if (as.POSIXlt(to)$mday > 1 || as.POSIXlt(to)$mon > 0) {
      to.year <- to.year + 1
    }
    years <- seq(from.year, to.year, by = by)
    as.Date(paste(years + 1900, "01-01", sep = "-"))
  } else {
    stop("unsupported unit")
  }
}

.pretty.POSIXlt <- function(dates,
                            ideal.num.breaks = 5,
                            upper.bound = 1.75,
                            lower.bount = 0.75) {
  all.units <- c("weeks", "days", "hours", "mins", "secs", "years", "months")
  date.range <- range(dates)
  diffs <- lapply(all.units,
                  function(x) .diffdate(date.range[2], date.range[1], units = x))
  jumps <- lapply(diffs, function(x) round(x / ideal.num.breaks))

  jumps <- structure(unlist(jumps), names = all.units)

  best.unit <- names(which.min(jumps[jumps > 0]))[1]

  .seq.date(date.range[1], date.range[2], units = best.unit)
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
