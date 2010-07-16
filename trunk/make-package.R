#!Rscript

source("jjplot.R")

.make.package <- function() {  
  package.skeleton("jjplot",
                   force = TRUE)
  
  sapply(c("jjplot/demo"), dir.create, showWarnings=FALSE)
  file.copy(c("DESCRIPTION", "NAMESPACE", "jjplot.Rd", "jjplot.demo.R",
              "jjplot-package.Rd", "jjplot.performance.R",
              "00Index"),
            c("jjplot/DESCRIPTION", "jjplot/NAMESPACE",
              "jjplot/man/jjplot.Rd", "jjplot/demo/jjplot.R",
              "jjplot/man/jjplot-package.Rd",
              "jjplot/demo/jjplot.performance.R",
              "jjplot/demo/00Index"),
            overwrite=TRUE)

  ## These are undocumented functions.  Fix them!
  unlink(c("jjplot/man/lsos.Rd",
           "jjplot/man/jjplot.geom.abline.Rd",
           "jjplot/man/jjplot.geom.area.Rd",
           "jjplot/man/jjplot.geom.bar.Rd",
           "jjplot/man/jjplot.geom.box.Rd",
           "jjplot/man/jjplot.geom.curve.Rd",
           "jjplot/man/jjplot.geom.hline.Rd",
           "jjplot/man/jjplot.geom.legend.Rd",
           "jjplot/man/jjplot.geom.line.Rd",
           "jjplot/man/jjplot.geom.map.Rd",           
           "jjplot/man/jjplot.geom.point.Rd",
           "jjplot/man/jjplot.geom.text.Rd",
           "jjplot/man/jjplot.geom.tile.Rd",
           "jjplot/man/jjplot.geom.vline.Rd",
           "jjplot/man/jjplot.stat.alpha.Rd",
           "jjplot/man/jjplot.stat.border.Rd",
           "jjplot/man/jjplot.stat.ccdf.Rd",
           "jjplot/man/jjplot.stat.color.Rd",
           "jjplot/man/jjplot.stat.cumsum.Rd",
           "jjplot/man/jjplot.stat.density.Rd",
           "jjplot/man/jjplot.stat.fit.Rd",
           "jjplot/man/jjplot.stat.fun.Rd",
           "jjplot/man/jjplot.stat.fun.df.Rd",
           "jjplot/man/jjplot.stat.fun.x.Rd",
           "jjplot/man/jjplot.stat.fun.xy.Rd",
           "jjplot/man/jjplot.stat.fun.y.Rd",
           "jjplot/man/jjplot.stat.group.Rd",
           "jjplot/man/jjplot.stat.hist.Rd",
           "jjplot/man/jjplot.stat.jitter.Rd",
           "jjplot/man/jjplot.stat.log.Rd",
           "jjplot/man/jjplot.stat.normalize.Rd",           
           "jjplot/man/jjplot.stat.quantile.Rd",
           "jjplot/man/jjplot.stat.shape.Rd",
           "jjplot/man/jjplot.stat.size.Rd",
           "jjplot/man/jjplot.stat.sort.Rd",
           "jjplot/man/jjplot.stat.table.Rd",
           "jjplot/man/jjplot.stat.transform.Rd",                      
           "jjplot/man/jjplot.scale.Rd",           
           "jjplot/man/jjplot.scale.Date.Rd",
           "jjplot/man/jjplot.scale.default.Rd",
           "jjplot/man/jjplot.scale.POSIXct.Rd",
           "jjplot/man/jjplot.scale.factor.Rd",
           "jjplot/man/jjplot.theme.Rd",
           "jjplot/man/jjplot.theme.facet.x.Rd",
           "jjplot/man/jjplot.theme.facet.y.Rd",
           "jjplot/man/jjplot.theme.top.strip.Rd",
           "jjplot/man/jjplot.theme.right.strip.Rd"))
}

.make.package()
