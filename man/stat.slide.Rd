\name{stat.slide}
\alias{stat.slide}
\alias{lines.stat.slide}
\alias{plot.stat.slide}
\alias{print.stat.slide}

\encoding{latin1}

\title{ Sliding statistics }
\description{
Statistical parameters are not constant along a time series: mean or variance can vary each year, or during particular intervals (radical or smooth changes due to a pollution, a very cold winter, a shift in the system behaviour, etc. Sliding statistics offer the potential to describe series on successive blocs defined along the space-time axis
}

\usage{
stat.slide(x, y, xcut=NULL, xmin=min(x), n=NULL, frequency=NULL,
        deltat=1/frequency, basic=FALSE, desc=FALSE, norm=FALSE,
        pen=FALSE, p=0.95)
\method{print}{stat.slide}(x, \dots) 
\method{plot}{stat.slide}(x, stat="mean", col=c(1, 2), lty=c(par("lty"), par("lty")),
        leg=FALSE, llab=c("series", stat), lpos=c(1.5, 10), xlab="time", ylab="y",
        main=paste("Sliding statistics"), \dots)
\method{lines}{stat.slide}(x, stat="mean", col=3, lty=1, \dots)
}
\arguments{
  \item{x}{ a vector with time data for \code{stat.slide()}, or a 'stat.slide' object
    for the methods }
  \item{y}{ a vector with observation at corresponding times }
  \item{xcut}{ a vector with the position in time of the breaks between successive blocs. \code{xcut=NULL} by default. In the later case, a vector with equally spaced blocs is constructed using \code{xmin}, \code{n} and \code{frequency} or \code{deltat}. If a value is provided for \code{xcut}, then it supersedes all these other parameters }
  \item{xmin}{ the minimal value in the time-scale to use for constructing a vector of equally spaced breaks }
  \item{n}{ the number of breaks to use }
  \item{frequency}{ the frequency of the breaks in the time-scale }
  \item{deltat}{ the bloc interval touse for constructing an equally-spaced break vector. \code{deltat} is 1/\code{frequency} }
  \item{basic}{ do we have to return basic statistics (by default, it is FALSE)? These are: the number of values (nbr.val), the number of null values (nbr.null), the number of missing values (nbr.na), the minimal value (min), the maximal value (max), the range (range, that is, max-min) and the sum of all non-missing values (sum) }
  \item{desc}{ do we have to return descriptive statistics (by default, it is FALSE)? These are: the median (median), the mean (mean), the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the \code{p} level, the variance (var), the standard deviation (std.dev) and the variation coefficient (coef.var) defined as the standard deviation divided by the mean }
  \item{norm}{ do we have to return normal distribution statistics (by default, it is FALSE)? the skewness coefficient g1 (skewness), its significant criterium (skew.2SE, that is, g1/2.SEg1; if skew.2SE > 1, then skewness is significantly different than zero), kurtosis coefficient g2 (kurtosis), its significant criterium (kurt.2SE, same remark than for skew.2SE), the statistic of a Shapiro-Wilk test of normality (normtest.W) and its associated probability (normtest.p) }
  \item{pen}{ do we have to return Pennington and other associated statistics (by default, it is FALSE)? pos.median, pos.mean, pos.var, pos.std.dev, respectively the median, the mean, the standard deviation and the variance, considering only non-null values; geo.mean, the geometric mean that is, the exponential of the mean of the logarithm of the observations, excluding null values. pen.mean, pen.var, pen.std.dev, pen.mean.var, respectively the mean, the variance, the standard deviation and the variance of the mean after Pennington's estimators (see \code{pennington()}) }
  \item{p}{ the probability level to use to calculate the confidence interval on the mean (CI.mean). By default, \code{p=0.95} }
  \item{stat}{ the statistic to plot on the graph. You can use "min", "max", "median", "mean" (by default), "pos.median", "pos.mean", "geo.mean" and "pen.mean". The other statistics cannot be superposed on the graph of the series in the current version of the function }
  \item{col}{ the colors to use to plot the initial series and the statistics, respectively. By default, \code{col=c(1,2)} }
  \item{lty}{ the style to use to draw the original series and the statistics. The default style is used if this argument is not provided }
  \item{leg}{ if \code{leg=TRUE}, a legend box is drawn on the graph }
  \item{llab}{ the labels to use for the legend. By default, it is "series" and the corresponding statistics provided in \code{stat}, respectively }
  \item{lpos}{ the position of the top-left corner (x,y) of the legend box in the graph coordinates. By default \code{lpos=c(1.5,10)} }
  \item{xlab}{ the label of the x-axis }
  \item{ylab}{ the label of the y-axis }
  \item{main}{ the main title of the graph}
  \item{\dots}{ additional parameters }
}
\details{
  Available statistics are the same as for \code{stat.desc()} and \code{stat.pen()}. The Shapiro-Wilk test of normality is not available yet in Splus and it returns 'NA' in this environment. If not a priori known, successive blocs can be identified using either \code{local.trend()} or \code{decmedian()} (see respective functions for further details)
}
\value{
  An object of type 'stat.slide' is returned. It has methods \code{print()}, \code{plot()} and \code{lines()}.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{stat.desc}}, \code{\link{stat.pen}}, \code{\link{pennington}}, \code{\link{local.trend}}, \code{\link{decmedian}} }

\examples{
data(marbio)
# Sliding statistics with fixed-length blocs
statsl <- stat.slide(1:68, marbio[, "ClausocalanusA"], xmin=0, n=7, deltat=10)
statsl
plot(statsl, stat="mean", leg=TRUE, lpos=c(55, 2500), xlab="Station",
        ylab="ClausocalanusA")

# More information on the series, with predefined blocs
statsl2 <- stat.slide(1:68, marbio[, "ClausocalanusA"],
        xcut=c(0, 17, 25, 30, 41, 46, 70), basic=TRUE, desc=TRUE, norm=TRUE,
        pen=TRUE, p=0.95)
statsl2
plot(statsl2, stat="median", xlab="Stations", ylab="Counts",
        main="Clausocalanus A")              # Median
lines(statsl2, stat="min")                   # Minimum
lines(statsl2, stat="max")                   # Maximum
lines(c(17, 17), c(-50, 2600), col=4, lty=2) # Cuts
lines(c(25, 25), c(-50, 2600), col=4, lty=2)
lines(c(30, 30), c(-50, 2600), col=4, lty=2)
lines(c(41, 41), c(-50, 2600), col=4, lty=2)
lines(c(46, 46), c(-50, 2600), col=4, lty=2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 2300, labels=c("Peripheral Zone", "D1",
        "C", "Front", "D2", "Central Zone")) # Labels
legend(0, 1900, c("series", "median", "range"), col=1:3, lty=1)
# Get cuts back from the object
statsl2$xcut
}
\keyword{ ts }
