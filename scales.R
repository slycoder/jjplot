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
    num.sizes <- 10
    size.levels <- seq(sqrt(min.size), sqrt(max.size), length.out = num.sizes)^2
    rr <- range(ss)
    function(z) { size.levels[round(num.sizes * (z - rr[1]) / (rr[2] - rr[1]))] }
  }          
}

.make.alpha.scale <- function(ss) {
  if (is.null(ss)) {
    function(z) { 1.0 }
  } else {
    num.alphas <- 10
    size.levels <- sqrt(seq(0, 1, length.out = num.alphas))
    rr <- range(ss)
    function(z) { size.levels[round(num.alphas * (z - rr[1]) / (rr[2] - rr[1]))] }
  }          
}
