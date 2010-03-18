\name{regspline}
\alias{regspline}

\encoding{latin1}

\title{ Regulation of a time series using splines }
\description{
  Transform an irregular time series in a regular time series, or fill gaps in regular time series using splines
}
\usage{
regspline(x, y=NULL, xmin=min(x), n=length(x),
        deltat=(max(x) - min(x))/(n - 1), rule=1, periodic=FALSE)
}
\arguments{
  \item{x}{ a vector with time in the irregular series. Missing values are allowed }
  \item{y}{ a vector of same length as \code{x} and holding observations at corresponding times }
  \item{xmin}{ allows to respecify the origin of time in the calculated regular time series. By default, the origin is not redefined and it is equivalent to the smallest value in \code{x} }
  \item{n}{ the number of observations in the regular time series. By default, it is the same number than in the original irregular time series (i.e., \code{length(x)} }
  \item{deltat}{ the time interval between two observations in the regulated time series }
  \item{rule}{ the rule to use for extrapolated values (outside of the range in the initial irregular time series) in the regular time series. With \code{rule=1} (by default), these entries are not calculated and get \code{NA}; with \code{rule=2}, these entries are extrapolated }
  \item{periodic}{ indicates if the time series should be considered as periodic (\code{periodic=TRUE}, first value must be equal to the last one). If this is the case, first and second derivates used to calculate spline segments around first and last observations use data in the other extreme of the series. In the other case (\code{periodic=FALSE} (by default), derivates for extremes observations are considered to be equal to zero }
}
\details{
  Missing values are interpolated using cubic splines between observed values.
}
\value{
  An object of type 'regul' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{hist()}, \code{extract()} and \code{specs()}.
}
\references{ 
Lancaster, P. & K. Salkauskas, 1986. \emph{Curve and surface fitting.} Academic Press, England, 280 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ This function uses \code{spline()} for internal calculations. However, interpolated values are not allowed to be higher than the largest initial observation or lower than the smallest one. }

\seealso{ \code{\link{regul}}, \code{\link{regarea}}, \code{\link{regconst}}, \code{\link{reglin}}, \code{\link{regul.screen}}, \code{\link{regul.adj}}, \code{\link{tseries}}, \code{\link{is.tseries}}, \code{\link[stats]{splinefun}} }

\examples{
data(releve)
reg <- regspline(releve$Day, releve$Melosul)
plot(releve$Day, releve$Melosul, type="l")
lines(reg$x, reg$y, col=2)
}
\keyword{ ts }
\keyword{ manip }
\keyword{ chron }
\keyword{ smooth }
