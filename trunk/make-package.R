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
}

.make.package()
