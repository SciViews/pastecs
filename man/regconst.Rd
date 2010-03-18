\name{regconst}
\alias{regconst}

\encoding{latin1}

\title{ Regulate a series using the constant value method }
\description{
  Transform an irregular time series in a regular time series, or fill gaps in regular time series using the constant value method
}
\usage{
regconst(x, y=NULL, xmin=min(x), n=length(x),
        deltat=(max(x) - min(x))/(n - 1), rule=1, f=0)
}
\arguments{
  \item{x}{ a vector with time in the irregular series. Missing values are allowed }
  \item{y}{ a vector of same length as \code{x} and holding observations at corresponding times }
  \item{xmin}{ allows to respecify the origin of time in the calculated regular time series. By default, the origin is not redefined and it is equivalent to the smallest value in \code{x} }
  \item{n}{ the number of observations in the regular time series. By default, it is the same number than in the original irregular time series (i.e., \code{length(x)} }
  \item{deltat}{ the time interval between two observations in the regulated time series }
  \item{rule}{ the rule to use for extrapolated values (outside of the range in the initial irregular time series) in the regular time series. With \code{rule=1} (by default), these entries are not calculated and get \code{NA}; with \code{rule=2}, these entries are extrapolated }
  \item{f}{ coefficient giving more weight to the left value (\code{f=0}, by default), to the right value (\code{f=}) or to a combination of these two observations (0 < f <1) }
}
\details{
  This is the simplest, but the less powerful regulation method. Interpolated values are calculated according to existing observations at left and at right as: x[reg] = x[right]*f + x[left]*(f-1), with 0 < f < 1.
}
\value{
  An object of type 'regul' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{hist()}, \code{extract()} and \code{specs()}.  
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ This function uses \code{approx()} for internal calculations }

\seealso{ \code{\link{regul}}, \code{\link{regarea}}, \code{\link{reglin}}, \code{\link{regspline}}, \code{\link{regul.screen}}, \code{\link{regul.adj}}, \code{\link{tseries}}, \code{\link{is.tseries}} }

\examples{
data(releve)
reg <- regconst(releve$Day, releve$Melosul)
plot(releve$Day, releve$Melosul, type="l")
lines(reg$x, reg$y, col=2)
}
\keyword{ ts }
\keyword{ manip }
\keyword{ chron }
\keyword{ smooth }

