\name{decevf}
\alias{decevf}

\encoding{latin1}

\title{ Time series decomposition using eigenvector filtering (EVF) }
\description{
  The eigenvector filtering decomposes the signal by applying a principal component analysis (PCA) on the original signal and a certain number of copies of it incrementally lagged, collected in a multivariate matrix. Reconstructing the signal using only the most representative eigenvectors allows filtering it.
}
\usage{
decevf(x, type="additive", lag=5, axes=1:2)
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{type}{ the type of model, either \code{type="additive"} (by default), or \code{type="multiplicative"} }
  \item{lag}{ The maximum lag used. A PCA is run on the matrix constituted by vectors lagged from 0 to \code{lag}. The defaulf value is 5, but a value corresponding to no significant autocorrelation, on basis of examination of the autocorrelation plot obtained by \code{acf} in the library 'ts' should be used (Lag at first time the autocorrelation curve crosses significance lines multiplied by the frequency of the series). }
  \item{axes}{ The principal axes to use to reconstruct the filtered signal. For instance, to use axes 2 and 4, use \code{axes=c(2,4)}. By default, the first two axes are considered (\code{axes=1:2}) }
}
\value{
  a 'tsd' object
}
\references{ 
Colebrook, J.M., 1978. \emph{Continuous plankton records: zooplankton and environment, North-East Atlantic and North Sea 1948-1975.} Oceanologica Acta, 1:9-23.

Ibanez, F. & J.C. Dauvin, 1988. \emph{Long-term changes (1977-1987) on a muddy fine sand Abra alba - Melinna palmate population community from the Western English Channel.} J. Mar. Prog. Ser., 49:65-81.

Ibanez, F., 1991. \emph{Treatment of data deriving from the COST 647 project on coastal benthic ecology: The within-site analysis.} In: B. Keegan (ed.) \emph{Space and time series data analysis in coastal benthic ecology.} Pp. 5-43.

Ibanez, F. & M. Etienne, 1992. \emph{Le filtrage des séries chronologiques par l'analyse en composantes principales de processus (ACPP).} J. Rech. Océanogr., 16:27-33.

Ibanez, F., J.C. Dauvin & M. Etienne, 1993. \emph{Comparaison des évolutions à long-terme (1977-1990) de deux peuplements macrobenthiques de la Baie de Morlaix (Manche Occidentale): relations avec les facteurs hydroclimatiques.} J. Exp. Mar. Biol. Ecol., 169:181-214.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{deccensus}}, \code{\link{decmedian}},  \code{\link{decdiff}}, \code{\link{decreg}}, \code{\link{decloess}} }

\examples{
data(releve)
melo.regy <- regul(releve$Day, releve$Melosul, xmin=9, n=87,
        units="daystoyears", frequency=24, tol=2.2, methods="linear",
        datemin="21/03/1989", dateformat="d/m/Y")
melo.ts <- tseries(melo.regy)
acf(melo.ts)
# Autocorrelation is not significant after 0.16
# That corresponds to a lag of 0.16*24=4 (frequency=24)
melo.evf <- decevf(melo.ts, lag=4, axes=1)
plot(melo.evf, col=c(1, 4, 2))
# A superposed graph is better in the present case
plot(melo.evf, col=c(1, 4), xlab="stations", stack=FALSE, resid=FALSE,
        lpos=c(0, 60000))
}
\keyword{ ts }
\keyword{ smooth }
