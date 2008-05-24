\name{decaverage}
\alias{decaverage}
\title{ Time series decomposition using a moving average }
\description{
  Decompose a single regular time series with a moving average filtering. Return a 'tsd' object. To decompose several time series at once, use \code{tsd()} with the argument \code{method="average"}
}
\usage{
decaverage(x, type="additive", order=1, times=1, sides=2, ends="fill",
        weights=NULL)
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{type}{ the type of model, either \code{type="additive"} (by default), or \code{type="multiplicative"} }
  \item{order}{ the order of the moving average (the window of the average being 2*order+1), centered around the current observation or at left of this observation depending upon the value of the \code{sides} argument. Weights are the same for all observations within the window. However, if the argument \code{weights} is provided, it supersedes \code{order}. One can also use \code{order="periodic"}. In this case, a deseasoning filter is calculated according to the value of \code{frequency} }
  \item{times}{ The number of times to apply the method (by default, once) }
  \item{sides}{ If 2 (by default), the window is centered around the current observation. If 1, the window is at left of the current observation (including it) }
  \item{ends}{ either "NAs" (fill first and last values that are not calculable with NAs), or "fill" (fill them with the average of observations before applying the filter, by default), or "circular" (use last values for estimating first ones and vice versa), or "periodic" (use entire periods of contiguous cycles, deseasoning) }
  \item{weights}{ a vector indicating weight to give to all observations in the window. This argument has the priority over \code{order} }
}
\details{
  This function is a wrapper around the \code{filter()} function and returns a 'tsd' object. However, it offers more methods to handle ends.
}
\value{
  A 'tsd' object
}
\references{
Kendall, M., 1976. \emph{Time-series.} Charles Griffin & Co Ltd. 197 pp.

Laloire, J.C., 1972. \emph{Méthodes du traitement des chroniques.} Dunod, Paris, 194 pp.

Malinvaud, E., 1978. \emph{Méthodes statistiques de l'économétrie.} Dunod, Paris. 846 pp.

Philips, L. & R. Blomme, 1973. \emph{Analyse chronologique.} Université Catholique de Louvain. Vander ed. 339 pp.
 }
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{deccensus}}, \code{\link{decdiff}}, \code{\link{decmedian}},  \code{\link{decevf}}, \code{\link{decreg}}, \code{\link{decloess}} }

\examples{
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decaverage(ClausoB.ts, order=2, times=10, sides=2, ends="fill")
plot(ClausoB.dec, col=c(1, 3, 2), xlab="stations")
# A stacked graph is more representative in this case
plot(ClausoB.dec, col=c(1, 3), xlab="stations", stack=FALSE, resid=FALSE,
        lpos=c(53, 4.3))
}
\keyword{ ts }
\keyword{ smooth }
