\name{jjplot}
\alias{jjplot}
\title{
  The workhorse plotting function for the jjplot plotting library.
}
\description{
    This is the main entry point for generating plots via the jjplot
    library.   It is typically caled by specifying a data frame on which
    to operate, along with aesthetic mappings followed by a sequence of
    statistic and geometry commands.
}
\usage{
jjplot(x, y = NULL, data, ...)
}
\arguments{
  \item{x}{
    An expression to map to the \var{x} values of the plot.
  }
  \item{y}{
    An optional expression to map to the \var{y} values of the plot.
  }
  \item{data}{
    A data frame in whose scope the data expressions (i.e., \code{x})
    should be evaluated. 
  }
  \item{\dots}{
    Additional parameters and data specification expressions, along
  with a sequence of jjplot statistics and geometry functions which
  generate the plot.
  }
}
\details{
  Full up-to-date documentation with examples can be found at:
  \url{http://code.google.com/p/jjplot/wiki/Documentation}
}
\value{
  The function is called only for its side effect (namely, a plot!).
}
\references{
  \url{http://code.google.com/p/jjplot/wiki/Documentation}
}
\author{
Jonathan Chang <jcone@princeton.edu>
Eytan Bakshy <ebakshy@gmail.com>  
}
\examples{
 ## See the demos for example usages.
 \dontrun{demo(jjplot)}

 ## See the following demo for speed tests.
 \dontrun{demo(jjplot.performance)}
}
%% \keyword{ ~kwd1 }
