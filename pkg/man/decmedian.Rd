\name{decmedian}
\alias{decmedian}
\title{ Time series decomposition using a running median }
\description{
  This is a nonlinear filtering method used to smooth, but also to segment a time series. The isolated peaks and pits are leveraged by this method.
}
\usage{
decmedian(x, type="additive", order=1, times=1, ends="fill")
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{type}{ the type of model, either \code{type="additive"} (by default), or \code{type="multiplicative"} }
  \item{order}{ the window used for the running median corresponds to 2*order + 1  }
  \item{times}{ the number of times the running median is applied. By default, 1 }
  \item{ends}{ the method used to calculate ends. Either "NAs" (fill extremes, non-calculable values with NAs), or "fill" (fill these extremes with the closest calculable median) }
}

\value{
  a 'tsd' object
}
\references{ 
Gebski, V.J., 1985. \emph{Some properties of splicing when applied to non-linear smoothers.} Comp. Stat. Data Anal., 3:151-157.

Philips, L. & R. Blomme, 1973. \emph{Analyse chronologique. Université Catholique de Louvain.} Vander ed. 339 pp.

Tukey, J.W., 1977. \emph{Exploratory Data Analysis.} Reading Massachusetts: Addison-Wesley.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{deccensus}}, \code{\link{decdiff}}, \code{\link{decevf}}, \code{\link{decreg}}, \code{\link{decloess}} }

\examples{
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decmedian(ClausoB.ts, order=2, times=10, ends="fill")
plot(ClausoB.dec, col=c(1, 4, 2), xlab="stations")
# This is a transect across a frontal zone:
plot(ClausoB.dec, col=c(0, 2), xlab="stations", stack=FALSE, resid=FALSE)
lines(c(17, 17), c(0, 10), col=4, lty=2)
lines(c(25, 25), c(0, 10), col=4, lty=2)
lines(c(30, 30), c(0, 10), col=4, lty=2)
lines(c(41, 41), c(0, 10), col=4, lty=2)
lines(c(46, 46), c(0, 10), col=4, lty=2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 8.7, labels=c("Peripheral Zone", "D1",
        "C", "Front", "D2", "Central Zone"))
}
\keyword{ ts }
\keyword{ smooth }
