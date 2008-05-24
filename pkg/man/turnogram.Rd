\name{turnogram}
\alias{turnogram}
\alias{extract.turnogram}
\alias{identify.turnogram}
\alias{plot.turnogram}
\alias{print.summary.turnogram}
\alias{print.turnogram}
\alias{summary.turnogram}
\title{ Calculate and plot a turnogram for a regular time series }
\description{
The turnogram is the variation of a monotony index with the observation scale (the number of data per time unit). A monotony index indicates if the series has more or less erratic variations than a pure random succession of independent observations. Since a time series almost always has autocorrelation, it is expected to be more monotonous than a purely random series. The monotony index is a way to quantify the density of information beared by a time series. The turnogram determines at which observation scale this density of information is maximum. It is also the scale that optimize the sampling effort (best compromise between less samples versus more information).
}
\synopsis{
turnogram(series, intervals=c(1, length(series)/5), step=1, complete=FALSE, two.tailed=TRUE, FUN=mean, plotit=TRUE, level=0.05, lhorz=TRUE, lvert=FALSE, xlog=TRUE)
extract.turnogram(e, n, level=e$level, FUN=e$fun, drop=0, ...)
identify.turnogram(x, lvert=TRUE, col=2, lty=2, ...)
plot.turnogram(x, level=0.05, lhorz=TRUE, lvert=TRUE, lcol=2, llty=2, xlog=TRUE, xlab=paste("interval (", x$units.text, ")", sep=""), ylab="I (bits)", main=paste(x$type, "turnogram for:", x$data), sub=paste(x$fun, "/", x$proba), ...)
print.summary.turnogram(x, ...)
print.turnogram(x, ...)
summary.turnogram(object, ...)
}
\usage{
turnogram(series, intervals=c(1, length(series)/5), step=1, complete=FALSE,
        two.tailed=TRUE, FUN=mean, plotit=TRUE, level=0.05, lhorz=TRUE,
        lvert=FALSE, xlog=TRUE)
\method{summary}{turnogram}(turno)
\method{plot}{turnogram}(turno, level=0.05, lhorz=TRUE, lvert=TRUE, lcol=2,
        llty=2, xlog=TRUE, \dots)
\method{identify}{turnogram}(turno, lvert=TRUE, \dots)
\method{extract}{turnogram}(turno, n, level=turno$level, FUN=turno$fun, drop=0)
}
\arguments{
  \item{series}{ a single regular time series ('rts' object in Splus or 'ts' object in \R) }
  \item{intervals}{ the range (mini, maxi) of the intervals to calculate, i.e., to take one obervation every 'interval' one. By default, \code{intervals} ranges from 1 to the fifth of the total number of observations }
  \item{step}{ the increment used for the intervals. By defaults \code{step=1}. To limit calculation or for a first screenning with a large range in the intervals, use a higher value for \code{step} }
  \item{complete}{ if \code{complete=TRUE}, a complete turnogram is calculated, showing mean, minimal and maximal curves. If it is FALSE (by default), only a simple turnogram always starting from the first observation is calculated }
  \item{two.tailed}{ if \code{two.tailed=TRUE} (by default), the monotony index is tested with a bilateral test, otherwise, a left-sided test is used }
  \item{FUN}{ a function to apply to aggregate data in the intervals. It is a function of the type \code{FUN(x, na.rm, \dots)}. The most used function is \code{mean()} (by default), but it is also possible to keep only the first value with \code{first()}, the last value with \code{last()}, the median or the sum of values in the interval. The later function is useful for cumulative observations, like pluviometry. It should be noted that the turnograms with \code{FUN=mean} and with \code{FUN=sum} are the same, but that extraction of final series are different for levels > 1 }
  \item{plotit}{ if \code{plotit=TRUE} (by default), the graph of the turnogram is also plotted }
  \item{level}{ the significant level to draw on the graph. By default \code{level=0.05}, corresponding to a test with P = 5\% }
  \item{lhorz}{ if \code{lhorz=TRUE} (by default) then one (left-sided test), or two (two-sided test) horizontal lines are drawn on the graph, indicating the significant level of the test given by the argument \code{level}. Any point above the single line, or outside the interval defined by the two lines is significant }
  \item{lvert}{ if \code{lvert=TRUE} (by default, except for \code{turnogram()} function), a vertical line is drawn, indicating the time interval that corresponds to the maximum information and it is also the automatic level of extraction unless this value is changed }
  \item{lcol}{ the color to use to draw supplemental lines: the horizontal line indicating where the test is significant (if \code{lhorz=TRUE}) and the vertical line indicating the extraction level (if \code{lvert=TRUE}). By default, color 2 is used }
  \item{llty}{ the style for the supplemental lines. By default, style 2 is used (dashed lines) }
  \item{xlog}{ if \code{xlog=TRUE} (by default), then the x-axis is expressed in logarithms. Otherwise, a linear scale is used }
  \item{\dots}{ additional optional graphic arguments }
  \item{turno}{ a 'turnogram' object as returned by the function \code{turnogram()} }
  \item{n}{ the number of observations to take into account in the initial series. Use \code{n=NULL} (by default) to use all observations of the series }
  \item{drop}{ the number of observations to drop at the beginning of the series before proceeding with the aggregation of the data for the extracted series. By default, \code{drop=0}: no observations are dropped }
}
\details{
The turnogram is a generalisation of the information theory (see \code{turnpoints()}). If a series has a lot of erratic peaks and pits that alternate with a high frequency, it is more difficult to interpret than a more monotonous series. These erratic fluctuations can be eliminated by changing the scale of observation (keeping one observation every two, three, four,... from the original series). The turnogram resample the original series this way, and calculate a monotony index for each resampled subseries. This monotony index quantifies the number of peaks and pits presents in the series, compared to the total number of observations. The Gleissberg distribution (see \code{pgleissberg()}) indicates the probability to have such a number of extrema in a series given it is purely random. It is possible to test monotony indices: is it a random series or not (two-sided test), or is more monotonous than a random series (left-sided test) thanks to a Chi-2 test proposed by Wallis & Moore (1941). 

There are various turnograms depending on the way the observations are aggregated inside each time interval. For instance, if one consider one observation every three from the original series, each group of three observations can be aggregated in several different ways. One can take the mean of the three observation, or the median value, or the sum,... One can also decide not to aggregate observations, but to drop some of them. Hence, one can take only the first or the last observation of the group. All these options can be choosen by defining the argument \code{FUN=...}. A simple turnogram correspond to the change of the monotony index with the scale of observation, stating always from the first observation. One could also decide to start from the second, or the third observation for an aggregation of the observations three by three... and result could be somewhat different. A complete turnogram investigates all possibles combinations (observation scale versus starting point for the aggregation) and trace the maximal, minimal and mean curves for the change of the monotony index. It is thus more informative than the simple turnogram. However, it takes much more time to compute.

The most obvious use of the turnogram is for the pretreatment of continuously sampled data. It helps in deciding which is the optimal sampling interval for the series to bear as most information as possible while keeping the dataset as small as possible. It is also interesting to compare the turnogram with other functions like the variogram (see \code{vario()}) or the spectrogram (see \code{spectrum()}).
}
\value{
  An object of type 'turnogram' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{identify()} and \code{extract()}.
}
\references{ 
Dallot, S. & M. Etienne, 1990. \emph{Une méthode non paramétrique d'analyse des series en océanographie biologique: les tournogrammes.} Biométrie et océanographie - Société de biométrie, 6, Lille, 26-28 mai 1986. IFREMER, Actes de colloques, 10:13-31.

Johnson, N.L. & Kotz, S., 1969. \emph{Discrete distributions.} J. Wiley & sons, New York, 328 pp.

Kendall, M.G., 1976. \emph{Time-series, 2nd ed.} Charles Griffin & co, London.

Wallis, W.A. & G.H. Moore, 1941. \emph{A significance test for time series.} National Bureau of Economic Research, tech. paper n°1.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{pgleissberg}}, \code{\link{turnpoints}}, \code{\link{first}}, \code{\link{last}}, \code{\link{vario}}, \code{\link[stats]{spectrum}} }

\examples{
data(bnr)
# Let's transform series 4 into a time series (supposing it is regular)
bnr4 <- as.ts(bnr[, 4])
plot(bnr4, type="l", main="bnr4: raw data", xlab="Time")
# A simple turnogram is calculated
bnr4.turno <- turnogram(bnr4)
summary(bnr4.turno)
# A complete turnogram confirms that "level=3" is a good value: 
turnogram(bnr4, complete=TRUE)
# Data with maximum info. are extracted (thus taking 1 every 3 observations)
bnr4.interv3 <- extract(bnr4.turno)
plot(bnr4, type="l", lty=2, xlab="Time")
lines(bnr4.interv3, col=2)
title("Original bnr4 (dotted) versus max. info. curve (plain)")
# Choose another level (for instance, 6) and extract the corresponding series
bnr4.turno$level <- 6
bnr4.interv6 <- extract(bnr4.turno)
# plot both extracted series on top of the original one
plot(bnr4, type="l", lty=2, xlab="Time")
lines(bnr4.interv3, col=2)
lines(bnr4.interv6, col=3)
legend(70, 580, c("original", "interval=3", "interval=6"), col=1:3, lty=c(2, 1, 1))
# It is hard to tell on the graph which series contains more information
# The turnogram shows us that it is the "interval=3" one!
}
\keyword{ ts }
\keyword{ htest }
