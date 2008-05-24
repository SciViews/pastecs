\name{tsd}
\alias{tsd}
\alias{extract.tsd}
\alias{plot.tsd}
\alias{print.specs.tsd}
\alias{print.summary.tsd}
\alias{print.tsd}
\alias{specs.tsd}
\alias{summary.tsd}
\title{ Decomposition of one or several regular time series using various methods }
\description{
  Use a decomposition method to split the series into two or more components. Decomposition methods are either series filtering/smoothing (difference, average, median, evf), deseasoning (loess) or model-based decomposition (reg, i.e., regression).
}
\synopsis{
tsd(x, specs=NULL, method="loess", type=if (method == "census") "multiplicative" else "additive", lag=1, axes=1:5, order=1, times=1, sides=2, ends="fill", weights=NULL, s.window=NULL, s.degree=0, t.window=NULL, t.degree=2, robust=FALSE, trend=FALSE, xreg=NULL) 
extract.tsd(e, n, series=NULL, components=NULL, ...)
plot.tsd(x, series=1, stack=TRUE, resid=TRUE, col=par("col"), lty=par("lty"), labels=dimnames(X)[[2]], leg=TRUE, lpos=c(0, 0), xlab="time", ylab="series", main=paste("Series decomposition by", x$specs$method, "-", x$specs$type), ...)
print.specs.tsd(x, ...)
print.summary.tsd(x, ...)
print.tsd(x, ...)
specs.tsd(x, ...)
summary.tsd(object, ...)
}
\usage{
tsd(x, specs=NULL, method="loess", type="additive", \dots)
\method{summary}{tsd}(tsdobj)
\method{plot}{tsd}(tsdobj, series=1, stack=TRUE, resid=TRUE,
        labels, leg=TRUE, lpos=c(0, 0), \dots)
\method{extract}{tsd}(tsdobj, n, series=NULL, components=NULL)
\method{specs}{tsd}(tsdobj)
}
\arguments{
  \item{x}{ an univariate or multivariate regular time series ('rts' in Splus or 'ts' in \R) to be decomposed }
  \item{specs}{ specifications are collected from a 'tsd' object, using the \code{specs} method. This allows for reusing parameters issued from a previous similar analysis }
  \item{method}{ the method to use to decompose the time series. Currently, possible values are: \code{"diff"}, \code{"average"}, \code{"median"}, \code{"evf"}, \code{"reg"}, \code{"loess"} (by default) or \code{"census"}. The corresponding function \code{decXXXX()} is applied to each of the series in \code{x} }
  \item{type}{ the type of model to use: either \code{"additive"} (by default) or \code{"multiplicative"}. In the additive model, all components must be added to reconstruct the initial series. In the multiplicative model, they must be multiplied (one components has the same unit as the original series, and the other ones are dimensionless multiplicative factors) }
  \item{\dots}{ (1) for \code{tsd()}: further arguments to pass to the corresponding \code{decXXXX()} function. (2) for \code{plot()}: further graphical arguments }
  \item{tsdobj}{ a 'tsd' object as returned by the function \code{tsd()}, or any of the \code{decXXXX()} functions }
  \item{series}{ (1) for \code{plot()}: the series to plot. By default, \code{series=1}, the first (or possibly unique) series in the 'tsd' object is plotted. (2) for \code{extract}: the name or the index of the series to extract. If \code{series} is provided, then \code{n} is ignored. By default, \code{series=NULL}. It is also possible to use negative indices. In this case, all series are extracted, except those ones }
  \item{stack}{ graphs of each component are either stacked (\code{stack=TRUE}, by default), or superposed on the same graph \code{stack=FALSE} }
  \item{resid}{ do we have to plot also the "residuals" components (\code{resid=TRUE}, by default) or not? Usually, in a stacked graph, you would like to plot the residuals, while in a superposed graph, you would not }
  \item{labels}{ the labels to use for all y-axes in a stacked graph, or in the legend for a superposed graph. By default, the names of the components ("trend", "seasonal", "deseasoned", "filtered", "residuals", ...) are used }
  \item{leg}{ only used when \code{stack=FALSE}. Do we plot a legend (\code{leg=TRUE} or not? }
  \item{lpos}{ position of the upper-left corner of the legend box in the graph coordinates (x,y). By default, \code{leg=c(0,0)} }
  \item{n}{ the number of series to extract (from series 1 to series n). By default, n equals the number of series in the 'tsd' object. If both \code{series} and \code{components} arguments are NULL, all series and components are extracted and this method has exactly the same effect as \code{tseries} }
  \item{components}{ the names or indices of the components to extract. If \code{components=NULL} (by default), then all components of the selected series are extracted. It is also possible to specify negative indices. In this case, all components are extracted, except those ones }
}
\details{
  To eliminate trend from a series, use "diff" or use "loess" with \code{trend=TRUE}. If you know the shape of the trend (linear, exponential, periodic, etc.), you can also use it with the "reg" (regression) method. To eliminate or extract seasonal components, you can use "loess" if the seasonal component is additive, or "census" if it is multiplicative. You can also use "average" with argument \code{order="periodic"} and with either an additive or a multiplicative model, although the later method is often less powerful than "loess" or "census". If you want to extract a seasonal cycle with a given shape (for instance, a sinusoid), use the "reg" method with a fitted sinusoidal equation. If you want to identify levels in the series, use the "median" method. To smooth the series, you can use preferably the "evf" (eigenvector filtering), or the "average" methods, but you can also use "median". To extract most important components from the series (no matter if they are cycles -seasonal or not-, or long-term trends), you should use the "evf" method. For more information on each of these methods, see online help of the corresponding \code{decXXXX()} functions.
}
\value{
  An object of type 'tsd' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{extract()} and \code{specs()}.
}
\references{ 
Kendall, M., 1976. \emph{Time-series.} Charles Griffin & Co Ltd. 197 pp.

Laloire, J.C., 1972. \emph{Méthodes du traitement des chroniques.} Dunod, Paris, 194 pp.

Legendre, L. & P. Legendre, 1984. \emph{Ecologie numérique. Tome 2: La structure des données écologiques.} Masson, Paris. 335 pp.

Malinvaud, E., 1978. \emph{Méthodes statistiques de l'économétrie.} Dunod, Paris. 846 pp.

Philips, L. & R. Blomme, 1973. \emph{Analyse chronologique.} Université Catholique de Louvain. Vander ed. 339 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ If you have to decompose a single time series, you could also use the corresponding \code{decXXXX()} function directly. In the case of a multivariate regular time series, \code{tsd()} is more convenient because it decompose all times series of a set at once! }

\seealso{ \code{\link{tseries}}, \code{\link{decdiff}}, \code{\link{decaverage}}, \code{\link{decmedian}}, \code{\link{decevf}}, \code{\link{decreg}}, \code{\link{decloess}}, \code{\link{deccensus}} }

\examples{
data(releve)
# Regulate the series and extract them as a time series object
rel.regy <- regul(releve$Day, releve[3:8], xmin=6, n=87, units="daystoyears",
        frequency=24, tol=2.2, methods="linear", datemin="21/03/1989",
        dateformat="d/m/Y")
rel.ts <- tseries(rel.regy)

# Decompose all series in the set with the "loess" method
rel.dec <- tsd(rel.ts, method="loess", s.window=13, trend=FALSE)
rel.dec
plot(rel.dec, series=5, col=1:3)    # An plot series 5

# Extract "deseasoned" components
rel.des <- extract(rel.dec, series=3:6, components="deseasoned")
rel.des[1:10,]

# Further decompose these components with a moving average
rel.des.dec <- tsd(rel.des, method="average", order=2, times=10)
plot(rel.des.dec, series=3, col=c(2, 4, 6))
# In this case, a superposed graph is more appropriate:
plot(rel.des.dec, series=3, col=c(2,4), stack=FALSE, resid=FALSE,
        labels=c("without season cycle", "trend"), lpos=c(0, 55000))
# Extract residuals from the latter decomposition
rel.res2 <- extract(rel.des.dec, components="residuals")
}
\keyword{ ts }
\keyword{ smooth }
\keyword{ loess }
\keyword{ nonparametric }
