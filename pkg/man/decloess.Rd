\name{decloess}
\alias{decloess}

\encoding{latin1}

\title{ Time series decomposition by the LOESS method }
\description{
  Compute a seasonal decomposition of a regular time series using a LOESS method (local polynomial regression)
}
\usage{
decloess(x, type="additive", s.window=NULL, s.degree=0, t.window=NULL,
        t.degree=2, robust=FALSE, trend=FALSE)
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{type}{ the type of model. This is for compatibility purpose. The only model type that is accepted for this method is \code{type="additive"}. For a multiplicative model, use \code{deccensus()} instead }
  \item{s.window}{ the width of the window used to extract the seasonal component. Use an odd value equal or just larger than the number of annual values (frequency of the time series). Use another value to extract other cycles (circadian, lunar,...). Using \code{s.window="periodic"} ensures a correct value for extracting a seasonal component when the time scale is in years units }
  \item{s.degree}{ the order of the polynome to use to extract the seasonal component (0 or 1). By default \code{s.degree=0} }
  \item{t.window}{ the width of the window to use to extract the general trend when \code{trend=TRUE} (indicate an odd value). If this parameter is not provided, a reasonable value is first calculated, and then used by the algorithm. }
  \item{t.degree}{ the order of the polynome to use to extract the general trend (0, 1 or 2). By default \code{t.degree=2} }
  \item{robust}{ if \code{TRUE} a robust regression method is used. Otherwise (\code{FALSE}), by default, a classical least-square regression is used }
  \item{trend}{ If \code{TRUE} a trend is calculated (under R only). Otherwise, the series is decomposed into a seasonal component and residuals only }
}
\details{
  This function uses the \code{stl()} function for the decomposition. It is a wrapper that create a 'tsd' object
}
\value{
  a 'tsd' object
}
\references{ 
Cleveland, W.S., E. Grosse & W.M. Shyu, 1992. \emph{Local regression models. Chapter 8 of Statistical Models in S.} J.M. Chambers & T.J. Hastie (eds). Wadsworth & Brook/Cole.

Cleveland, R.B.,  W.S. Cleveland, J.E. McRae, & I. Terpenning, 1990. \emph{STL: A seasonal-trend  decomposition  procedure based on loess.} J. Official Stat., 6:3-73.
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{deccensus}}, \code{\link{decmedian}}, \code{\link{decdiff}}, \code{\link{decevf}}, \code{\link{decreg}} }

\examples{
data(releve)
melo.regy <- regul(releve$Day, releve$Melosul, xmin=9, n=87,
        units="daystoyears", frequency=24, tol=2.2, methods="linear",
        datemin="21/03/1989", dateformat="d/m/Y")
melo.ts <- tseries(melo.regy)
melo.dec <- decloess(melo.ts, s.window="periodic")
plot(melo.dec, col=1:3)
}
\keyword{ ts }
\keyword{ smooth }
