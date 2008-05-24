\name{AutoD2}
\alias{AutoD2}
\alias{CrossD2}
\alias{CenterD2}
\title{ AutoD2, CrossD2 or CenterD2 analysis of a multiple time-series }
\description{
  Compute and plot multiple autocorrelation using Mahalanobis generalized distance D2. AutoD2 uses the same multiple time-series. CrossD2 compares two sets of multiple time-series having same size (same number of descriptors). CenterD2 compares subsamples issued from a single multivariate time-series, aiming to detect discontinuities.
}
\usage{
AutoD2(series, lags=c(1, nrow(series)/3), step=1, plotit=TRUE,
        add=FALSE, ...)
CrossD2(series, series2, lags=c(1, nrow(series)/3), step=1,
        plotit=TRUE, add=FALSE, ...)
CenterD2(series, window=nrow(series)/5, plotit=TRUE, add=FALSE,
        type="l", level=0.05, lhorz=TRUE, lcol=2, llty=2, ...)
}
\arguments{
  \item{series}{ regularized multiple time-series }
  \item{series2}{ a second set of regularized multiple time-series }
  \item{lags}{ minimal and maximal lag to use. By default, 1 and a third of the number of observations in the series respectively }
  \item{step}{ step between successive lags. By default, 1 }
  \item{window}{ the window to use for CenterD2. By default, a fifth of the total number of observations in the series }
  \item{plotit}{ if \code{TRUE} then also plot the graph }
  \item{add}{ if \code{TRUE} then the graph is added to the current figure }
  \item{type}{ The type of line to draw in the CenterD2 graph. By default, a line without points }
  \item{level}{ The significance level to consider in the CenterD2 analysis. By default 5\% }
  \item{lhorz}{ Do we have to plot also the horizontal line representing the significance level on the graph? }
  \item{lcol}{ The color of the significance level line. By default, color 2 is used }
  \item{llty}{ The style for the significance level line. By default: \code{llty=2}, a dashed line is drawn}
  \item{\dots}{ additional graph parameters }
}
\value{
  An object of class 'D2' which contains:
  \item{lag }{The vector of lags}
  \item{D2 }{The D2 value for this lag}
  \item{call }{The command invoked when this function was called}
  \item{data }{The series used}
  \item{type }{The type of 'D2' analysis: 'AutoD2', 'CrossD2' or 'CenterD2'}
  \item{window }{The size of the window used in the CenterD2 analysis}
  \item{level }{The significance level for CenterD2}
  \item{chisq }{The chi-square value corresponding to the significance level in the CenterD2 analysis}
  \item{units.text }{Time units of the series, nicely formatted for graphs}
}
\references{
Cooley, W.W. & P.R. Lohnes, 1962. \emph{Multivariate procedures for the behavioural sciences.} Whiley & sons.

Dagnélie, P., 1975. \emph{Analyse statistique à plusieurs variables.} Presses Agronomiques de Gembloux.

Ibanez, F., 1975. \emph{Contribution à l'analyse mathématique des évènements en écologie planctonique: optimisations méthodologiques; étude expérimentale en continu à petite échelle du plancton côtier.} Thèse d'état, Paris VI.

Ibanez, F., 1976. \emph{Contribution à l'analyse mathématique des évènements en écologie planctonique. Optimisations méthodologiques.} Bull. Inst. Océanogr. Monaco, 72:1-96.

Ibanez, F., 1981. \emph{Immediate detection of heterogeneities in continuous multivariate oceanographic recordings. Application to time series analysis of changes in the bay of Villefranche sur mer.} Limnol. Oceanogr., 26:336-349.

Ibanez, F., 1991. \emph{Treatment of the data deriving from the COST 647 project on coastal benthic ecology: The within-site analysis.} In: B. Keegan (ed), \emph{Space and time series data analysis in coastal benthic ecology}, p 5-43.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\section{WARNING }{If data are too heterogeneous, results could be biased (a singularity matrix appears in the calculations).} 

\seealso{ \code{\link[base]{mahalanobis}}, \code{\link[stats]{acf}} }

\examples{
data(marphy)
marphy.ts <- as.ts(as.matrix(marphy[, 1:3]))
AutoD2(marphy.ts)
marphy.ts2 <- as.ts(as.matrix(marphy[, c(1, 4, 3)]))
CrossD2(marphy.ts, marphy.ts2)
# This is not identical to:
CrossD2(marphy.ts2, marphy.ts)
marphy.d2 <- CenterD2(marphy.ts, window=16)
lines(c(17, 17), c(-1, 15), col=4, lty=2)
lines(c(25, 25), c(-1, 15), col=4, lty=2)
lines(c(30, 30), c(-1, 15), col=4, lty=2)
lines(c(41, 41), c(-1, 15), col=4, lty=2)
lines(c(46, 46), c(-1, 15), col=4, lty=2)
text(c(8.5, 21, 27.5, 35, 43.5, 57), 11, labels=c("Peripheral Zone", "D1",
        "C", "Front", "D2", "Central Zone")) # Labels
time(marphy.ts)[marphy.d2$D2 > marphy.d2$chisq]
}

\keyword{ ts }
\keyword{ multivariate }
\keyword{ htest }
