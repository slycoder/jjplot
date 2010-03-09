### SCALES ###
.match.scale <- function(override, group, scales,
                        defaults = list(color = "black",
                                        size = 1.0,
                                        fill = "black"),
                        type = c("color", "size", "fill")) {
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
    if (is.null(manual)) {
      cr <- colorRamp(c('red', 'white', 'blue'))
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
