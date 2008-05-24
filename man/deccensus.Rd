\name{deccensus}
\alias{deccensus}
\title{ Time decomposition using the CENSUS II method }
\description{
  The CENSUS II method allows to decompose a regular time series into a trend, a seasonal component and residuals, according to a multiplicative model
}
\usage{
deccensus(x, type="multiplicative", trend=FALSE)
}
\arguments{
  \item{x}{ A single regular time serie (a 'rts' object under S+ and a 'ts' object under R) with a "years" time scale (one unit = one year) and a complete number of cycles (at least 3 complete years) }
  \item{type}{ The type of model. This is for compatibility with other \code{decxxx()} functions, but only a multiplicative model is allowed here }
  \item{trend}{ If \code{trend=TRUE} a trend component is also calculated, otherwise, the decomposition gives only a seasonal component and residuals }
}
\details{
  The trend component contains both a general trend and long-term oscillations. The seasonal trend may vary from year to year. For a seasonal decomposition using an additive model, use \code{decloess()} instead
}
\value{
  a 'tsd' object
}
\references{ 
Béthoux, N., M. Etienne, F. Ibanez & J.L. Rapaire, 1980. \emph{Spécificités hydrologiques des zones littorales. Analyse chronologique par la méthode CENSUS II et estimation des échanges océan-atmosphère appliqués à la baie de Villefranche sur mer.} Ann. Inst. Océanogr. Paris, 56:81-95.

Fromentin, J.M. & F. Ibanez, 1994. \emph{Year to year changes in meteorological features on the French coast area during the last half-century. Examples of two biological responses.} Oceanologica Acta, 17:285-296.

Institut National de Statistique de Belgique, 1965. \emph{Décomposition des séries chronologiques en leurs composantes suivant différentes méthodes. Etudes statistiques et économétriques.} Bull. Stat. INS, 10:1449-1524.

Philips, J. & R. Blomme, 1973. \emph{Analyse chronologique.} Université Catholique de Louvain, Vander ed. 339 pp.

Rosenblatt, H.M., 1968. \emph{Spectral evaluation of BLS and CENSUS revised seasonal adjustment procedures.} J. Amer. Stat. Assoc., 68:472-501.

Shiskin, J. & H. Eisenpress, 1957. \emph{Seasonal adjustment by electronic computer methods.} J. Amer. Stat. Assoc., 52:415-449.

}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{decdiff}}, \code{\link{decmedian}}, \code{\link{decevf}}, \code{\link{decreg}}, \code{\link{decloess}} }

\examples{
data(releve)
# Get regulated time series with a 'years' time-scale
rel.regy <- regul(releve$Day, releve[3:8], xmin=6, n=87, units="daystoyears", frequency=24, tol=2.2, methods="linear", datemin="21/03/1989", dateformat="d/m/Y")
rel.ts <- tseries(rel.regy)
# We must have complete cycles to allow using deccensus()
start(rel.ts)
end(rel.ts)
rel.ts2 <- window(rel.ts, end=c(1992,5))
rel.dec2 <- deccensus(rel.ts2[, "Melosul"], trend=TRUE)
plot(rel.dec2, col=c(1,4,3,2))
}
\keyword{ ts }
\keyword{ smooth }
