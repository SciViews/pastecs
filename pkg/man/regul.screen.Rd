\name{regul.screen}
\alias{regul.screen}

\encoding{latin1}

\title{ Test various regulation parameters }
\description{
  Seek for the best combination of the number of observation, the interval between two successive observation and the position of the first observation in the regulated time series to match as much observations of the initial series as possible
}

\usage{
regul.screen(x, weight=NULL, xmin=min(x), frequency=NULL,
    deltat=(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))/(length(x) - 1),
    tol=deltat/5, tol.type="both")
}
\arguments{
  \item{x}{ a vector with times corresponding to the observations in the irregular initial time series }
  \item{weight}{ a vector of the same length as \code{x}, with the weight to give to each observation. A value of 0 indicates to ignore an observation. A value of 1 gives a normal weight to an observation. A higher value gives more importance to the corresponding observation. You can increase weight of observations around major peaks and pits, to make sure they are not lost in the regulated time series. If \code{weight=NULL} (by default), then a weight of 1 is used for all observations }
  \item{xmin}{ a vector with all time values for the first observation in the regulated time series to be tested }
  \item{frequency}{ a vector with all the frequencies to be screened }
  \item{deltat}{ a vector with all time intervals to screen. \code{deltat} is the inverse of \code{frequency}. Only one of these two arguments is required. If both are provided, \code{frequency} supersedes \code{deltat} }
  \item{tol}{ it is possible to tolerate some differences in the time between two matching observations (in the original irregular series and in the regulated series). If \code{tol=0} both values must be strictly identical; a higher value allows some fuzzy matching. \code{tol} must be a round fraction of \code{deltat} and cannot be higher than it, otherwise, it is adjusted to the closest acceptable value. By default, \code{tol=deltat/5} }
  \item{tol.type}{ the type of window to use for the time-tolerance: \code{"left"}, \code{"right"}, \code{"both"} (by default) or \code{"none"}. If \code{tol.type="left"}, corresponding \code{x} values are seeked in a window ]xregul-tol, xregul]. If \code{tol.type="right"}, they are seeked in the window [xregul, xregul+tol[. If \code{tol.type="both"}, then they are seeked in the window ]xregul-tol, xregul+tol]. If several observations are in this window, the closest one is used. Finally, if \code{tol.type="none"}, then \emph{all} observations in the regulated time series are interpolated (even if exactly matching observations exist!) }
}
\details{
  Whatever the efficiency of the interpolation procedure used to regulate an irregular time series, a matching, non-interpolated observation is always better than an interpolated one! With very irregular time series, it is often difficult to decide which is the better regular time-scale in order to interpolate as less observations as possible. \code{regul.screen()} tests various combinations of number of observation, interval between two observations and position of the first observation and allows to choose the combination that best matches the original irregular time series. To choose also an optimal value for \code{tol}, use \code{regul.adj()} concurrently.
}
\value{
  A list containing:
  \item{tol}{ a vector with the adjusted values of \code{tol} for the various values of \code{deltat} }
  \item{n}{ a table indicating the maximum value of \code{n} for all combinations of \code{deltat} and \code{xmin} to avoid any extrapolation }
  \item{nbr.match}{ a table indicating the number of matching observations (in the tolerance window) for all combinations of \code{deltat} and \code{xmin} }
  \item{nbr.exact.match}{ a table indicating the number of exactly matching observations (with a tolerance window equal to zero) for all combinations of \code{deltat} and \code{xmin} }
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{regul.adj}}, \code{\link{regul}} }

\examples{
data(releve)
# This series is very irregular, and it is difficult
# to choose the best regular time-scale
releve$Day
length(releve$Day)
intervals <- releve$Day[2:61]-releve$Day[1:60]
intervals
range(intervals)
mean(intervals)
# A combination of xmin=1, deltat=22 and n=61 seems correct
# But is it the best one?
regul.screen(releve$Day, xmin=0:11, deltat=16:27, tol=1.05)
# Now we can tell that xmin=9, deltat=21, n=63, with tol=1.05
# is a much better choice! 
}
\keyword{ ts }
\keyword{ chron }
