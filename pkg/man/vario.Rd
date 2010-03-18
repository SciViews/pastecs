\name{vario}
\alias{vario}

\encoding{latin1}

\title{ Compute and plot a semi-variogram }
\description{
  Compute a classical semi-variogram for a single regular time series
}
\usage{
vario(x, max.dist=length(x)/3, plotit=TRUE, vario.data=NULL)
}
\arguments{
  \item{x}{ a vector or an univariate time series }
  \item{max.dist}{ the maximum distance to calculate. By default, it is the third of the number of observations }
  \item{plotit}{ If \code{plotit=TRUE} then the graph of the semi-variogram is plotted }
  \item{vario.data}{ data coming from a previous call to \code{vario()}. Call the function again with these data to plot the corresponding graph }
}
\value{
  A data frame containing distance and semi-variogram values
}
\references{ 
David, M., 1977. \emph{Developments in geomathematics. Tome 2: Geostatistical or reserve estimation.} Elsevier Scientific, Amsterdam. 364 pp.

Delhomme, J.P., 1978. \emph{Applications de la théorie des variables régionalisées dans les sciences de l'eau.} Bull. BRGM, section 3 n°4:341-375.

Matheron, G., 1971. \emph{La théorie des variables régionalisées et ses applications.} Cahiers du Centre de Morphologie Mathématique de Fontainebleau. Fasc. 5 ENSMP, Paris. 212 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{disto}} }

\examples{
data(bnr)
vario(bnr[, 4])
}
\keyword{ ts }
