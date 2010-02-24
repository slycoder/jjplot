#!Rscript

source("jjplot.R")

.make.package <- function() {  
  package.skeleton("jjplot",
                   c("jjplot"),
                   force = TRUE)
  
  sapply(c("jjplot/demo"), dir.create, showWarnings=FALSE)
  file.copy(c("DESCRIPTION", "NAMESPACE", "jjplot.Rd", "jjplot.demo.R",
              "jjplot-package.Rd"),
            c("jjplot/DESCRIPTION", "jjplot/NAMESPACE",
              "jjplot/man/jjplot.Rd", "jjplot/demo/jjplot.R",
              "jjplot/man/jjplot-package.Rd"),
            overwrite=TRUE)
}

.make.package()
