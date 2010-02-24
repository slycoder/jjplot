\name{jjplot}
\alias{jjplot}
\title{
  The workhorse plotting function for the jjplot plotting library.
}
\description{
  The main entry 
}
\usage{
jjplot(x = NULL, data = NULL, ...)
}
\arguments{
  \item{x}{
    
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
