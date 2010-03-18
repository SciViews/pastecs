\name{turnpoints}
\alias{turnpoints}
\alias{extract.turnpoints}
\alias{lines.turnpoints}
\alias{plot.turnpoints}
\alias{print.summary.turnpoints}
\alias{print.turnpoints}
\alias{summary.turnpoints}

\encoding{latin1}

\title{ Analyze turning points (peaks or pits) }
\description{
  Determine the number and the position of extrema (turning points, either peaks or pits) in a regular time series. Calculate the quantity of information associated to the observations in this series, according to Kendall's information theory
}
\synopsis{
turnpoints(x)
extract.turnpoints(e, n, no.tp=0, peak=1, pit=-1, ...)
lines.turnpoints(x, max=TRUE, min=TRUE, median=TRUE, col=c(4, 4, 2), lty=c(2, 2, 1), ...)
plot.turnpoints(x, level=0.05, lhorz=TRUE, lcol=2, llty=2, type="l", xlab="data number", ylab=paste("I (bits), level = ", level * 100, "\%", sep = ""), main=paste("Information (turning points) for:", x$data), ...)
print.summary.turnpoints(x, ...)
print.turnpoints(x, ...)
summary.turnpoints(object, ...)
}
\usage{
turnpoints(x)
\method{summary}{turnpoints}(turnp)
\method{plot}{turnpoints}(turnp, level=0.05, lhorz=TRUE, lcol=2, llty=2, \dots)
\method{lines}{turnpoints}(turnp, max=TRUE, min=TRUE, median=TRUE,
        col=c(4, 4, 2), lty=c(2, 2, 1), \dots)
\method{extract}{turnpoints}(turnp, n, no.tp=0, peak=1, pit=-1)
}
\arguments{
  \item{x}{ a vector or a time series }
  \item{turnp}{ a 'turnpoints' object, as returned by the function \code{turnpoints()} }
  \item{level}{ the significant level to draw on the graph if \code{lhorz=TRUE}. By default, \code{level=0.05}, which corresponds to a 5\% p-value for the test }
  \item{lhorz}{ if \code{lhorz=TRUE} (by default), an horizontal line indicating significant level is drawn on the graph }
  \item{lcol}{ the color to use to draw the significant level line, by default, color 2 is used }
  \item{llty}{ the style to use for the significant level line. By default, style 2 is used (dashed line) }
  \item{\dots}{ Additional graph parameters }
  \item{max}{ do we plot the maximum envelope line (by default, yes) }
  \item{min}{ do we plot the minimum envelope line (by default, yes) }
  \item{median}{ do we plot the median line inside the envelope (by default, yes) }
  \item{col}{ a vector of three values for the color of the max, min, median lines, respectively. By default \code{col=c(4,4,2)} }
  \item{lty}{ a vector of three values for the style of the max, min, median lines, respectively. By default \code{lty=c(2,2,1)}, that is: dashed, dashed and plain lines }
  \item{n}{ the number of points to extract. By default \code{n=length(turnp)}, all points are extracted }
  \item{no.tp}{ extract gives a vector representing the position of extrema in the original series. \code{no.tp} represents the code to use for points that are not an extremum, by default '0' }
  \item{peak}{ the code to use to flag a peak, by default '1' }
  \item{pit}{ the code to use to flag a pit, by default '-1' }
}
\details{
  This function tests if the time series is purely random or not. Kendall (1976) proposed a series of tests for this. Moreover, graphical methods using the position of the turning points to draw automatically envelopes around the data are implemented, and also the drawing of median points between these envelopes.
  
  With a purely random time series, one expect to find, on average, a turning point (peak or pit that is, an observation that is preceeded and followed by, respectively, lower or higher observations) every 1.5 observation. Given it is impossible to determine if first and last observation are turning point, it gives:

  \deqn{E(p) = 2/3*(n-2)}

  with p, the number of observed turning points and n the number of observations. The variance of p is:

  \deqn{var(p) = (16*n - 29)/90}

  Ibanez (1982) demonstrated that P(t), the probability to observe a turning point at time t is:

  \deqn{P(t) = 2*(1/n(t-1)! * (n-1)!)}

  where P is the probability to observe a turning point at time t under the null hypothesis that the time series is purely random, and thus, the distribution of turning points follows a normal distribution.

  The quantity of information I associated with this probability is:  

  \deqn{I = -log2 P(t)}

  It can be interpreted as follows. If I is larger, there are less turning points than expected in a purely random series. There are, thus, longer sequence of increasing or decreasing values along the time scale. This is considered to be more informative.

As you can easily imagine, from this point on, it is straightforward to construct a test to determine if the series is random (regarding the distribution of the turning points), more or less monotonic (more or less turning points than expected).
}

\value{
  An object of type 'turnpoints' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()} and \code{extract()}.
Regarding your specific question, 'info' is the quantity of information I associated with the turning points:
  \item{data}{ The dataset to which the calculation is done }
  \item{n}{ The number of observations }
  \item{points}{ The value of the points in the series, after elimination of ex-aequos }
  \item{pos}{ The position of the points on the time scale in the series (including ex-aequos) }
  \item{exaequos}{ Location of exaequos (1), or not (0) }
  \item{nturns}{ Total number of tunring points in the whole time series }
  \item{firstispeak}{ Is the first turning point a peak (\code{TRUE}), or not (\code{FALSE}) }
  \item{peaks}{ Logical vector. Location of the peaks in the time series without ex-aequos }
  \item{pits}{ Logical vector. Location of the pits in the time series without ex-aequos}
  \item{tppos}{ Position of the turning points in the initial series (with ex-aequos) }
  \item{proba}{ Probability to find a turning point at this location (see details) }
  \item{info}{ Quantity of information associated with this point (see details) }
}

\references{ 
Ibanez, F., 1982. \emph{Sur une nouvelle application de la theorie de l'information à la description des series chronologiques planctoniques.} J. Exp. Mar. Biol. Ecol., 4:619-632

Kendall, M.G., 1976. \emph{Time-series, 2nd ed.} Charles Griffin & Co, London.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\section{WARNING }{ the \code{lines()} method should be used to draw lines on the graph of the \emph{original} dataset (\code{plot(data, type="l")} for instance), \emph{not} on the graph of turning points (\code{plot(turnp)})! }

\seealso{ \code{\link{turnogram}}, \code{\link{stat.slide}} }

\examples{
data(marbio)
plot(marbio[, "Nauplii"], type="l")
# Calculate turning points for this series
Nauplii.tp <- turnpoints(marbio[, "Nauplii"])
summary(Nauplii.tp)
plot(Nauplii.tp)
# Add envelope and median line to original data
plot(marbio[, "Nauplii"], type="l")
lines(Nauplii.tp)
# Note that lines() applies to the graph of original dataset!!!
title("Raw data, envelope maxi., mini. and median line")
}
\keyword{ ts }
