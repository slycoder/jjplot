\name{jjplot}
\alias{jjplot}
\title{
  The workhorse plotting function for the jjplot plotting library.
}
\description{
    This is the main entry point for generating plots via the jjplot
    library.   It is called by specifying a data frame on which
    to operate, along with a formula which defines aesthetic mappings
    along with a tree of statistic and geometry commands.
}
\usage{
jjplot(f, data = NULL, color = NULL,
       fill = NULL, size = NULL, alpha = 1.0,
       log.x = FALSE, log.y = FALSE,
       xlab = NULL, ylab = NULL,
       expand = c(0.04, 0.04),
       color.scale = NULL)
}
\arguments{
  \item{f}{
    A formula specifying mappings for x and y aesthetics, as well as the
    statistic and geometry commands.  See details. 
  }
  \item{data}{
    A data frame in whose scope the data expressions (i.e., \code{f})
    should be evaluated. 
  }
  \item{color}{
    The expression to map to the color aesthetic.
  }
  \item{fill}{
    The expression to map to the fill aesthetic.
  }
  \item{size}{
    The expression to map to the size aesthetic.
  }
  \item{alpha}{
    A constant numeric value with which to specify the alpha blending
used on color and fill scales.
  }
  \item{log.x}{
    Use a logarithmic x scale.
  }
  \item{log.y}{
    Use a logarithmic y scale.
  }
  \item{xlab}{
    When specified, this overrides the default label for the x axis.
  }
  \item{ylab}{
    When specified, this overrides the default label for the y axis.
  }
  \item{expand}{ A length-2 numeric vector.  The axes are guaranteed to
    contain the range of the data plus a fraction of the data range
    specified by this parameter.  The default simulates the default
    behavior of R's base plotting functions.  }
  \item{color.scale}{
    A function which maps values between 0 and 1 to colors.  If
specified this color scale will override the default automatically
generated color scale.
  }
}
\details{
  The formula is specified by placing the optional y aesthetic to the
  left of the '~' and the x aesthetic as 
  the first term (rightmost) of the formula.  Layers are subsequently
  evaluated from right to left, separated by '+'.  Parentheses are
  grouping operators for layers.  Statistics can be applied using the
  ':' operator, with the statistic on the right hand side and operations
  depending on the statistic on the left hand side.

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
