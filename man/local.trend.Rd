\name{local.trend}
\alias{local.trend}
\alias{identify.local.trend}

\encoding{latin1}

\title{ Calculate local trends using cumsum }
\description{
  A simple method using cumulated sums that allows to detect changes in the
  tendency in a time series
}
\usage{
local.trend(x, k=mean(x), plotit=TRUE, type="l", cols=1:2, ltys=2:1,
    xlab="Time", ylab="cusum", \dots)
\method{identify}{local.trend}(x, \dots)
}
\arguments{
  \item{x}{ a regular time series (a 'ts' object) for \code{local.trend()} or a
    'local.trend' object for \code{identify()} }
  \item{k}{ the reference value to substract from cumulated sums. By default, it
    is the mean of all observations in the series }
  \item{plotit}{ if \code{plotit=TRUE} (by default), a graph with the cumsum
    curve superposed to the original series is plotted }
  \item{type}{ the type of plot (as usual notation for this argument) }
  \item{cols}{ colors to use for original data and for the trend line }
  \item{ltys}{ line types to use for original data and the trend line }
  \item{xlab}{ label of the x-axis }
  \item{ylab}{ label of the y-axis }
  \item{\dots}{ additional arguments for the graph }
}
\details{
  With \code{local.trend()}, you can:
  
  - detect changes in the mean value of a time series
  
  - determine the date of occurence of such changes
  
  - estimate the mean values on homogeneous intervals
}
\value{
  a 'local.trend' object is returned. It has the method \code{identify()}
}
\references{ 
Ibanez, F., J.M. Fromentin & J. Castel, 1993. \emph{Application de la méthode
  des sommes cumulées à l'analyse des séries chronologiques océanographiques.}
  C. R. Acad. Sci. Paris, Life Sciences, 316:745-748.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\note{
  Once transitions are identified with this method, you can use
  \code{stat.slide()} to get more detailed information on each phase. A
  smoothing of the series using running medians (see \code{decmedian()}) allows
  also to detect various levels in a time series, but according to the median
  statistic. Under \R, see also the 'strucchange' package for a more complete,
  but more complex, implementation of cumsum applied to time series. }

\seealso{ \code{\link{trend.test}}, \code{\link{stat.slide}},
  \code{\link{decmedian}} }

\examples{
data(bnr)
# Calculate and plot cumsum for the 8th series
bnr8.lt <- local.trend(bnr[,8])
# To identify local trends, use:
# identify(bnr8.lt)
# and click points between which you want to compute local linear trends...
}
\keyword{ ts }
