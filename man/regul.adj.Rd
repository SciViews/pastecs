\name{regul.adj}
\alias{regul.adj}

\encoding{latin1}

\title{ Adjust regulation parameters }
\description{
  Calculate and plot an histogram of the distances between interpolated observations in a regulated time series and closest observations in the initial irregular time series. This allows to optimise the \code{tol} parameter
}

\usage{
regul.adj(x, xmin=min(x), frequency=NULL,
     deltat=(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))/(length(x) - 1),
     tol=deltat, tol.type="both", nclass=50, col=c(4, 5, 2),
     xlab=paste("Time distance"), ylab=paste("Frequency"),
     main="Number of matching observations", plotit=TRUE, \dots)
}
\arguments{
  \item{x}{ a vector with times corresponding to the observations in the irregular initial time series }
  \item{xmin}{ the time corresponding to the first observation in the regular time series }
  \item{frequency}{ the frequency of observations in the regular time series }
  \item{deltat}{ the interval between two successive observations in the regular time series. This is the inverse of \code{frequency}. Only one of both parameters need to be given. If both are provided, \code{frequency} supersedes \code{deltat} }
  \item{tol}{ the tolerance in the difference between two matching observations (in the original irregular series and in the regulated series). If \code{tol=0} both values must be strictly identical; a higher value for \code{tol} allows some fuzzy matching. \code{tol} must be a round fraction of \code{deltat} and cannot be higher than it, otherwise, it is adjusted to the closest acceptable value. By default, \code{tol=deltat} }
  \item{tol.type}{ the type of window to use for the time-tolerance: \code{"left"}, \code{"right"}, \code{"both"} (by default) or \code{"none"}. If \code{tol.type="left"}, corresponding \code{x} values are seeked in a window ]xregul-tol, xregul]. If \code{tol.type="right"}, they are seeked in the window [xregul, xregul+tol[. If \code{tol.type="both"}, then they are seeked in the window ]xregul-tol, xregul+tol]. If several observations are in this window, the closest one is used. Finally, if \code{tol.type="none"}, then \emph{all} observations in the regulated time series are interpolated (even if exactly matching observations exist!) }
  \item{nclass}{ the number of classes to compute in the histogram. This is indicative, and will be adjusted by the algorithm to produce a nicely-formatted histogram. The default value is \code{nclass=50}. It is acceptable in many cases, but if the histogram is not correct, try a larger value }
  \item{col}{ the three colors to use to represent respectively the fist bar (exact coincidence), the middle bars (coincidence in a certain tolerance window) and the last bar (values always interpolated). By default, \code{col=c(4,5,2)} }
  \item{xlab}{ the label of the x-axis }
  \item{ylab}{ the label of the y-axis }
  \item{main}{ the main title of the graph}
  \item{plotit}{ if \code{plotit=TRUE} then the histogram is plotted. Otherwise, it is only calculated }
  \item{\dots}{ additional graph parameters for the histogram }
}
\details{
  This function is complementary to \code{regul.screen()}. While the later look for the best combination of the number of observations, the interval between observations and the position of the first observation on the time-scale for the regular time series, \code{regul.adj()} look for the optimal value for \code{tol}, the tolerance window.
}
\value{
  A list with components:
  \item{params}{ the parameters used for the regular time-scale }
  \item{match}{ the number of matching observations in the tolerance window }
  \item{exact.match}{ the number of exact matching observations }
  \item{match.counts}{ a vector with the number of matching observations for increasing values of \code{tol} }
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{regul.screen}}, \code{\link{regul}} }

\examples{
# This example follows the example for regul.screen()
# where we determined that xmin=9, deltat=21, n=63, with tol=1.05
# is a good choice to regulate the irregular time series in 'releve' 
data(releve)
regul.adj(releve$Day, xmin=9, deltat=21)
# The histogram indicates that it is not useful to increase tol
# more than 1.05, because few observations will be added
# except if we increase it to 5-7, but this value could be
# considered to be too large in comparison with deltat=22
# On the other hand, with tol <= 1, the number of matching
# observations will be almost divided by two!
}
\keyword{ ts }
\keyword{ chron }
