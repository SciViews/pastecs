\name{regul}
\alias{regul}
\alias{extract.regul}
\alias{hist.regul}
\alias{identify.regul}
\alias{lines.regul}
\alias{plot.regul}
\alias{print.regul}
\alias{print.specs.regul}
\alias{print.summary.regul}
\alias{specs.regul}
\alias{summary.regul}

\encoding{latin1}

\title{ Regulation of one or several time series using various methods }
\description{
  Regulate irregular time series or regular time series with gaps. Create a \code{regul} object from whose one or several regular time series can be extracted using \code{extract()} or \code{tseries()}. This is the function to apply most of the time to create regular time series ('rts' objects in Splus or 'ts' objects in \R) that will be further analyzed by other functions that apply to regular time series.
}
\synopsis{
regul(x, y=NULL, xmin=min(x), n=length(x), units="days", frequency=NULL, deltat=1/frequency, datemin=NULL, dateformat="m/d/Y", tol=NULL, tol.type="both", methods="linear", rule=1, f=0, periodic=FALSE, window=(max(x) - min(x))/(n - 1), split=100, specs=NULL) 
extract.regul(e, n, series=NULL, ...)
hist.regul(x, nclass=30, col=c(4, 5, 2), xlab=paste("Time distance in", x$units, "with start =", min(x$x), , ylab=paste("Frequency, tol =", x$specs$tol), main="Number of matching observations", plotit=TRUE, ...)
identify.regul(x, series=1, col=3, label="#", ...)
lines.regul(x, series=1, col=3, lty=1, plot.pts=TRUE, ...)
plot.regul(x, series=1, col=c(1, 2), lty=c(par("lty"), par("lty")), plot.pts=TRUE, leg=FALSE, llab=c("initial", x$specs$methods[series]), lpos=c(1.5, 10), xlab=paste("Time (", x$units, ")", sep = ""), ylab="Series", main=paste("Regulation of", names(x$y)[series]), ...)
print.regul(x, ...)
print.specs.regul(x, ...)
print.summary.regul(x, ...)
specs.regul(x, ...)
summary.regul(object, ...)
}
\usage{
regul(x, y=NULL, xmin=min(x), n=length(x), units="days", frequency=NULL,
        deltat=1/frequency, datemin=NULL, dateformat="m/d/Y", tol=NULL,
        tol.type="both", methods="linear", rule=1, f=0, periodic=FALSE,
        window=(max(x) - min(x))/(n - 1), split=100, specs=NULL)
\method{summary}{regul}(reg)
\method{plot}{regul}(reg, series=1, col=c(1, 2), lty, plot.pts=TRUE,
        leg=FALSE, llab=c("initial", x$specs$methods[series]),
        lpos=c(1.5, 10), \dots)
\method{lines}{regul}(reg, series=1, col=3, lty=1, plot.pts=TRUE, \dots)
\method{identify}{regul}(reg, series=1, col=3, label="#", \dots)
\method{hist}{regul}(reg, nclass=30, col=c(4, 5, 2), plotit=TRUE, \dots)
\method{extract}{regul}(reg, n=ncol(reg$y), series=NULL)
\method{specs}{regul}(reg)
}
\arguments{
  \item{x}{ a vector containing times at which observations are sampled in the initial irregular time series. It can be expressed in any unit ("years", "days", "weeks", "hours", "min", "sec",...) as defined by the argument \code{units}. It is often expressed in "days" and the decimal part represents the part of the day, that is the time in hour:min:sec (dates coming from Excel, or even standard dates in S+ or \R are expressed like that) }
  \item{y}{ a vector (single series) or a matrix/data frame whose columns correspond to the various irregular time series to regulate. Rows are observations made at corresponding times in \code{x}. The number of rows must thus match the length of vector \code{x} }
  \item{xmin}{ allows to respecify the origin of time in \code{x}. By default, the origin is not redefined and thus, the smallest value in \code{x} is used }
  \item{n}{ the number of observations in the regular time series. By default, it is the same number than in the original irregular time series (i.e., \code{length(x)} }
  \item{units}{ the time unit for the \code{x} vector. By default \code{units="days"}. A special value, \code{units="daystoyears"} indicates that \code{x} is expressed in "days" (1 unit = 1 day) but that we want to obtain the final regular time series expressed in "years" (1 unit = 1 year). Give a correct value for \code{datemin} to make sure the right fraction of the year is computed for each observation (see example hereunder) }
  \item{frequency}{ the frequency of the regulated time series in the corresponding time unit. For instance, \code{frequency=12} with \code{units="years"} means montly sampled observations. Warning! When using \code{units="daystoyears"}, specify \code{frequency} (or \code{deltat}) in years! }
  \item{deltat}{ the interval between two observations in the regulated time series. It is the inverse of \code{frequency}. If both \code{frequency} and \code{deltat} are provided, then \code{frequency} supersedes \code{deltat} }
  \item{datemin}{ this is mostly useful for converting "days" in "years" time-scales (\code{units="daystoyears"}). If the \code{x} vector contains: 1, 3, 6,... (day \#1, day \#3, day \#6,... of the experiment), one can give here the exact date of the first observation, allowing to define a correct origin in the "years" time scale. Provide a string in a format compatible with \code{dateformat}. For instance, if day \#1 is the 21th March 1998, give \code{datemin="03/21/1998"} with \code{dateformat="m/d/Y"} }
  \item{dateformat}{ indicate how \code{datemin} is formated. For instance: \code{"d/m/Y"}, or \code{"m/d/Y"} (by default), see \code{daystoyears()} for more info on date formatting }
  \item{tol}{ the tolerance in the time-scale to determine if a measured value is used to approximate a regulated value. If \code{tol=0}, observations in each respective series must match exactly, otherwise observations in the regulated series are interpolated. \code{tol} must be a round fraction of \code{deltat} (deltat, deltat/2, deltat/3, etc...), and cannot be larger than it, otherwise, \code{tol} is automatically adjusted to the closest allowed value. By default, \code{tol=NULL}. This is equivalent to \code{tol=0}. Warning! In the particular case of \code{units="daystoyears"}, \code{tol} must be expressed in the original time-scale, that is "days", while \code{deltat} must be expressed in the fimal time-scale, that is "years"! }
  \item{tol.type}{ the type of adjustment to use for the time-tolerance: \code{"left"}, \code{"right"}, \code{"both"} (by default) or \code{"none"}. If \code{tol.type="left"}, corresponding \code{x} values are seeked in a window ]xregul-tol, xregul]. If \code{tol.type="right"}, they are seeked in the window [xregul, xregul+tol[. If \code{tol.type="both"}, then they are seeked in the window ]xregul-tol, xregul+tol]. If several observations are in this window, the closest one is used. Finally, if \code{tol.type="none"}, then \emph{all} observations in the regulated time series are interpolated (even if exactly matching observations exist!) }
  \item{methods}{ the method(s) to use to regulate the time series. Currently, it can be: \code{"constant"}, \code{"linear"}, \code{"spline"} or \code{"area"} (or a unique abbreviation of them). If several time series are provided (\code{y} is a matrix or a data frame), it is possible to define methods individually for each series. For instance, \code{methods=c("l", "a", "s")} defines the "linear" method for the first series, the "area" method for the second one, the "spline" method for the third one,... and again the "linear" for the fourth, the "area" for the fifth one, etc. (recycling rule). By default, the "linear" method is selected for all series }
  \item{rule}{ the rule to use for extrapolated values (observations in the final regular time series that are outside the range of observed values in the initial time series). With \code{rule=1} (by default), these entries are not calculated and get NA; with \code{rule=2}, these entries are extrapolated (avoid using this option, or use with extreme care!!!) }
  \item{f}{ parameter for the \code{"constant"} regulation method. Coefficient giving more weight to the observation at left (\code{f=0}, by default), to the observation at right (\code{f=1}), or give an intermediate weight to both of these observations (0 < f < 1) during the interpolation (see \code{reglin()} }
  \item{periodic}{ parameter for the \code{"spline"} regulation method. Indicate if the time series should be considered as periodic (\code{periodic=TRUE}, first value must be equal to the last one). If this is the case, first and second derivates used to calculate spline segments around first and last observations use data in the other extreme of the series. In the other case (\code{periodic=FALSE}, by default), derivates for extremes observations are considered to be equal to zero }
  \item{window}{ parameter for the \code{"area"} regulation method. Size of the window to consider (see \code{regarea()}). By default, the mean interval between observations in the initial irregular time series is used. Give the same value as for deltat for working with adjacent windows }
  \item{split}{ other parameter for the \code{"area"} method. To optimise calculation time and to avoid to saturate memory, very long time series are splitted into smaller subunits (see \code{regarea()}). This is transparent for the user. The default value of \code{split=100} should be rarely changed. Give a lower value if the program fails and reports a memory problem during calculation }
  \item{specs}{ a \code{specs.regul} object returned by the function \code{specs()} applied to a \code{regul} object. Allows to collect parameterization of the \code{regul()} function and to apply them to another regulation }
  \item{reg}{ A \code{regul} object as obtained after using the \code{regul()} function }
  \item{series}{ the series to plot. By default, \code{series=1}, corresponding to the first (or possibly the unique) series in the \code{regul} object }
  \item{col}{ (1) for \code{plot()}: the two colors to use to draw respectively the initial irregular series and the final regulated series. \code{col=c(1,2)} by default. (2) for \code{hist()}: the three colors to use to represent respectively the fist bar (exact coincidence), the middle bars (coincidence in a certain tolerance window) and the last bar (values always interpolated). By default, \code{col=c(4,5,2)} }
  \item{lty}{ the style to use to draw lines for the initial series and the regulated series, respectively. The default style is used for both lines if this argument is not provided }
  \item{plot.pts}{ if \code{plot.pts=TRUE} (by default) then points are also drawn for the regulated series (+). Those points that match observations in the initial irregular series, and are not interpolated, are further marked with a circle }
  \item{leg}{ do we add a legend to the graph? By default, \code{leg=FALSE}, no legend is added }
  \item{llab}{ the labels to use for the initial irregular and the final regulated series, respectively. By default, it is \code{"initial"} for the first one and the name of the regulation method used for the second one (see \code{methods} argument) }
  \item{lpos}{ the position of the top-left corner of the legend box (x,y), in the graph coordinates }
  \item{\dots}{ additional graph parameters }
  \item{label}{ the character to use to mark points interactively selected on the graph. By default, \code{label="#"} }
  \item{nclass}{ the number of classes to calculate in the histogram. This is indicative and this value is automatically adjusted to obtain a nicely-formatted histogram. By default, \code{nclass=30} }
  \item{plotit}{ If \code{plotit=TRUE} then the histogram is plotted. Otherwise, it is only calculated }
}
\details{
  Several irregular time series (for instance, contained in a data frame) can be treated at once. Specify a vector with \code{"constant"}, \code{"linear"}, \code{"spline"} or \code{"area"} for the argument \code{methods} to use a different regulation method for each series. See corresponding fonctions (\code{regconst()}, \code{reglin()}, \code{regspline()} and \code{regarea()}), respectively, for more details on these methods. Arguments can be saved in a \code{specs} object and reused for other similar regulation processes. Functions \code{regul.screen()} and \code{regul.adj()} are useful to chose best time interval in the computed regular time series. If you want to work on seasonal effects in the time series, you will better use a "years" time-scale (1 unit = 1 year), or convert into such a scale. If initial time unit is "days" (1 unit = 1 day), a conversion can be operated at the same time as the regulation by specifying \code{units="daystoyears"}.
}
\value{
  An object of type 'regul' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{hist()}, \code{extract()} and \code{specs()}.
}
\references{ 
Lancaster, P. & K. Salkauskas, 1986. \emph{Curve and surface fitting.} Academic Press, England, 280 pp.

Fox, W.T. & J.A. Brown, 1965. \emph{The use of time-trend analysis for environmental interpretation of limestones.} J. Geol., 73:510-518.

Ibanez, F., 1991. \emph{Treatment of the data deriving from the COST 647 project on coastal benthic ecology: The within-site  analysis.} In: B. Keegan (ed). Space and Time Series Data Analysis in Coastal Benthic Ecology. Pp 5-43.

Ibanez, F. & J.C. Dauvin, 1988. \emph{Long-term changes (1977-1987) on a muddy fine sand Abra alba - Melinna palmata population community from the Western English Channel.} J. Mar. Ecol. Prog. Ser., 49:65-81.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\seealso{ \code{\link{regul.screen}}, \code{\link{regul.adj}}, \code{\link{tseries}}, \code{\link{is.tseries}}, \code{\link{regconst}}, \code{\link{reglin}}, \code{\link{regspline}}, \code{\link{regarea}}, \code{\link{daystoyears}} }

\examples{
data(releve)
# The series in this data frame are very irregularly sampled in time:
releve$Day
length(releve$Day)
intervals <- releve$Day[2:61]-releve$Day[1:60]
intervals
range(intervals)
mean(intervals)
# The series must be regulated to be converted in a 'rts' or 'ts object
rel.reg <- regul(releve$Day, releve[3:8], xmin=9, n=63, deltat=21,
        tol=1.05, methods=c("s","c","l","a","s","a"), window=21)
rel.reg
plot(rel.reg, 5)
specs(rel.reg)
# Now we can extract one or several regular time series
melo.ts <- extract(rel.reg, series="Melosul")
is.tseries(melo.ts)

# One can convert time-scale from "days" to "years" during regulation
# This is most useful for analyzing seasonal cycles in a second step
melo.regy <- regul(releve$Day, releve$Melosul, xmin=6, n=87,
        units="daystoyears", frequency=24, tol=2.2, methods="linear",
        datemin="21/03/1989", dateformat="d/m/Y")
melo.regy
plot(melo.regy, main="Regulation of Melosul")
# In this case, we have only one series in 'melo.regy'
# We can use also 'tseries()' instead of 'extract()'
melo.tsy <- tseries(melo.regy)
is.tseries(melo.tsy)
}
\keyword{ ts }
\keyword{ manip }
\keyword{ chron }
\keyword{ smooth }
