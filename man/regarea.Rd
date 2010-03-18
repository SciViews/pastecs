\name{regarea}
\alias{regarea}

\encoding{latin1}

\title{ Regulate a series using the area method }
\description{
  Transform an irregular time series in a regular time series, or fill gaps in regular time series using the area method
}
\usage{
regarea(x, y=NULL, xmin=min(x), n=length(x),
        deltat=(max(x) - min(x))/(n - 1), rule=1,
        window=deltat, interp=FALSE, split=100)
}
\arguments{
  \item{x}{ a vector with time in the irregular series. Missing values are allowed }
  \item{y}{ a vector of same length as \code{x} and holding observations at corresponding times }
  \item{xmin}{ allows to respecify the origin of time in the calculated regular time series. By default, the origin is not redefined and it is equivalent to the smallest value in \code{x} }
  \item{n}{ the number of observations in the regular time series. By default, it is the same number than in the original irregular time series (i.e., \code{length(x)} }
  \item{deltat}{ the time interval between two observations in the regulated time series }
  \item{rule}{ the rule to use for extrapolated values (outside of the range in the initial irregular time series) in the regular time series. With \code{rule=1} (by default), these entries are not calculated and get \code{NA}; with \code{rule=2}, these entries are extrapolated }
  \item{window}{ size of the window to use for interpolation. By default, adjacent windows are used (\code{window=deltat}) }
  \item{interp}{ indicates if matching observations in both series must be calculated (\code{interp=TRUE}), or if initial observations are used "as is" in the final regular series (\code{interp=FALSE}, by default) }
  \item{split}{ a parameter to optimise calculation time and to avoid saturation of the memory. Very long time series are splitted into smaller subunits. This is transparent for the user. The default value of \code{split=100} should be rarely changed. Give a lower value if the program fails and reports a memory problem during calculation }
}
\details{
  This is a linear interpolation method described by Fox (1972). It takes into account all observations located in a given time window around the missing observation. On the contrary to many other interpolations (constant, linear, spline), the interpolated curve does not pass by the initial observations. Indeed, a smoothing is also operated simultaneously by this method. The importance of the smoothing is dependent on the size of the window (the largest it is, the smoothest will be the calculated regular time series) 
}
\value{
  An object of type 'regul' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{hist()}, \code{extract()} and \code{specs()}.
}
\references{ 
Fox, W.T. & J.A. Brown, 1965. \emph{The use of time-trend analysis for environmental interpretation of limestones.} J. Geol., 73:510-518.

Ibanez, F., 1991. \emph{Treatment of the data deriving from the COST 647 project on coastal benthic ecology: The within-site  analysis.} In: B. Keegan (ed). Space and Time Series Data Analysis in Coastal Benthic Ecology. Pp 5-43.

Ibanez, F. & J.C. Dauvin, 1988. \emph{Long-term changes (1977-1987) on a muddy fine sand Abra alba - Melinna palmata population community from the Western English Channel.} J. Mar. Ecol. Prog. Ser., 49:65-81.
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{regul}}, \code{\link{regconst}}, \code{\link{reglin}}, \code{\link{regspline}}, \code{\link{regul.screen}}, \code{\link{regul.adj}}, \code{\link{tseries}}, \code{\link{is.tseries}} }

\examples{
data(releve)
# The 'Melosul' series is regulated with a 25-days window
reg <- regarea(releve$Day, releve$Melosul, window=25)
# Then with a 50-days window
reg2 <- regarea(releve$Day, releve$Melosul, window=50)
# The initial and both regulated series are shown on the graph for comparison
plot(releve$Day, releve$Melosul, type="l")
lines(reg$x, reg$y, col=2)
lines(reg2$x, reg2$y, col=4)
}
\keyword{ ts }
\keyword{ manip }
\keyword{ chron }
\keyword{ smooth }
