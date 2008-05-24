\name{decdiff}
\alias{decdiff}
\title{ Time series decomposition using differences (trend elimination) }
\description{
  A filtering using X(t+lag) - X(t) has the property to eliminate the general trend from the series, whatever its shape
}
\usage{
decdiff(x, type="additive", lag=1, order=1, ends="fill")
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{type}{ the type of model, either \code{type="additive"} (by default), or \code{type="multiplicative"} }
  \item{lag}{ The lag between the two observations used to calculate differences. By default, \code{lag=1} }
  \item{order}{ The order of the difference corresponds to the number of times it is applied, by default \code{order=1} }
  \item{ends}{ either "NAs" (fill first values that are not calculable with NAs), or "fill" (fill them with the average of following observations before applying the filter, by default), or "drop" (do not fill them). If \code{ends="drop"}, the filtered series will be shorter than the initial one by lag*order. In all other cases, the filtered series is as large as the initial one }
}
\details{
  This function is a wrapper around the \code{diff()} function to create a 'tsd' object. It also manages initial values through the \code{ends} argument.
}
\value{
  a 'tsd' object
}
\references{ 
Kendall, M., 1976. \emph{Time-series.} Charles Griffin & Co Ltd. 197 pp.

Laloire, J.C., 1972. \emph{Méthodes du traitement des chroniques.} Dunod, Paris, 194 pp.

Philips, L. & R. Blomme, 1973. \emph{Analyse chronologique.} Université Catholique de Louvain. Vander ed. 339 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{deccensus}}, \code{\link{decmedian}},  \code{\link{decevf}}, \code{\link{decreg}}, \code{\link{decloess}} }

\examples{
data(marbio)
ClausoB.ts <- ts(log(marbio$ClausocalanusB + 1))
ClausoB.dec <- decdiff(ClausoB.ts, lag=1, order=2, ends="fill")
plot(ClausoB.dec, col=c(1, 4, 2), xlab="stations")
}
\keyword{ ts }
\keyword{ smooth }
