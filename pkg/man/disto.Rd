\name{disto}
\alias{disto}

\encoding{latin1}

\title{ Compute and plot a distogram }
\description{
  A distogram is an extension of the variogram to a multivariate time-series. It computes, for each observation (with a constant interval h between each observation), the euclidean distance normated to one (chord distance)
}
\usage{
disto(x, max.dist=nrow(x)/4, plotit=TRUE, disto.data=NULL)
}
\arguments{
  \item{x}{ a matrix, a data frame or a multiple time-series }
  \item{max.dist}{ the maximum distance to calculate. By default, it is the third of the number of observations (that is, the number of rows in the matrix) }
  \item{plotit}{ If \code{plotit=TRUE} then the graph of the distogram is plotted }
  \item{disto.data}{ data coming from a previous call to \code{disto()}. Call the function again with these data to plot the corresponding graph }
}
\value{
  A data frame containing distance and distogram values
}
\references{ 
Dauvin, J.C. & F. Ibanez, 1986. \emph{Variations à long-terme (1977-1985) du peuplement des sables fins de la Pierre Noire (baie de Morlaix, Manche Occidentale): analyse statistique de l'évolution structurale.} Hydrobiologia, 142:171-186.

Ibanez, F. & J.C. Dauvin, 1988. \emph{Long-term changes (1977-1987) in a muddy fine sand Abra alba - Melinna palmate community from the Western English Channel: multivariate time-series analysis.} Mar. Ecol. Prog. Ser., 49:65-81.

Mackas, D.L., 1984. \emph{Spatial autocorrelation of plankton community composition in a continental shelf ecosystem.} Limnol. Ecol., 20:451-471.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{vario}} }

\examples{
data(bnr)
disto(bnr)
}
\keyword{ multivariate }
\keyword{ ts }
