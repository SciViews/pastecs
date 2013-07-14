\name{tsd}
\alias{tsd}
\alias{extract.tsd}
\alias{plot.tsd}
\alias{print.specs.tsd}
\alias{print.summary.tsd}
\alias{print.tsd}
\alias{specs.tsd}
\alias{summary.tsd}

\encoding{latin1}

\title{ Decomposition of one or several regular time series using various methods }
\description{
  Use a decomposition method to split the series into two or more components.
  Decomposition methods are either series filtering/smoothing (difference,
  average, median, evf), deseasoning (loess) or model-based decomposition (reg,
  i.e., regression).
}

\usage{
tsd(x, specs=NULL, method="loess",
    type=if (method == "census") "multiplicative" else "additive",
    lag=1, axes=1:5, order=1, times=1, sides=2, ends="fill", weights=NULL,
    s.window=NULL, s.degree=0, t.window=NULL, t.degree=2, robust=FALSE,
    trend=FALSE, xreg=NULL)
\method{print}{tsd}(x, \dots)
\method{summary}{tsd}(object, \dots)
\method{print}{summary.tsd}(x, \dots)
\method{plot}{tsd}(x, series=1, stack=TRUE, resid=TRUE, col=par("col"),
    lty=par("lty"), labels=dimnames(X)[[2]], leg=TRUE, lpos=c(0, 0), xlab="time",
    ylab="series", main=paste("Series decomposition by", x$specs$method, "-",
    x$specs$type), \dots)
\method{extract}{tsd}(e, n, series=NULL, components=NULL, \dots)
\method{specs}{tsd}(x, \dots)
\method{print}{specs.tsd}(x, \dots)
}

\arguments{
  \item{x}{ an univariate or multivariate regular time series ('ts' object) to
    be decomposed for \code{tsd()}, or a 'tsd' object for the methods }
  \item{specs}{ specifications are collected from a 'tsd' object, using the
    \code{specs} method. This allows for reusing parameters issued from a
    previous similar analysis }
  \item{method}{ the method to use to decompose the time series. Currently,
    possible values are: \code{"diff"}, \code{"average"}, \code{"median"},
    \code{"evf"}, \code{"reg"}, \code{"loess"} (by default) or \code{"census"}.
    The corresponding function \code{decXXXX()} is applied to each of the series
    in \code{x} }
  \item{type}{ the type of model to use: either \code{"additive"} (by default)
    or \code{"multiplicative"}. In the additive model, all components must be
    added to reconstruct the initial series. In the multiplicative model, they
    must be multiplied (one components has the same unit as the original series,
    and the other ones are dimensionless multiplicative factors) }
  \item{lag}{ The lag between the two observations used to calculate differences.
    By default, \code{lag=1} }
  \item{axes}{ the number of axes to show in the plot }
  \item{order}{ (1) for the method 'difference': the order of the difference
    corresponds to the number of times it is applied, by default \code{order=1},
    (2) for the method 'average': the order of the moving average (the window of
    the average being 2*order+1), centered around the current observation or at
    left of this observation depending upon the value of the \code{sides}
    argument. Weights are the same for all observations within the window.
    However, if the argument \code{weights} is provided, it supersedes
    \code{order}. One can also use \code{order="periodic"}. In this case, a
    deseasoning filter is calculated according to the value of \code{frequency}}
  \item{times}{ The number of times to apply the method (by default, once) }
  \item{sides}{ If 2 (by default), the window is centered around the current
    observation. If 1, the window is at left of the current observation
    (including it) }
  \item{ends}{ either "NAs" (fill first and last values that are not calculable
    with NAs), or "fill" (fill them with the average of observations before
    applying the filter, by default), or "circular" (use last values for
    estimating first ones and vice versa), or "periodic" (use entire periods of
    contiguous cycles, deseasoning) }
  \item{weights}{ a vector indicating weight to give to all observations in the
  window. This argument has the priority over \code{order} }
  \item{s.window}{ the width of the window used to extract the seasonal
    component. Use an odd value equal or just larger than the number of annual
    values (frequency of the time series). Use another value to extract other
    cycles (circadian, lunar,...). Using \code{s.window="periodic"} ensures a
    correct value for extracting a seasonal component when the time scale is in
    years units }
  \item{s.degree}{ the order of the polynome to use to extract the seasonal
    component (0 or 1). By default \code{s.degree=0} }
  \item{t.window}{ the width of the window to use to extract the general trend
    when \code{trend=TRUE} (indicate an odd value). If this parameter is not
    provided, a reasonable value is first calculated, and then used by the
    algorithm. }
  \item{t.degree}{ the order of the polynome to use to extract the general trend
    (0, 1 or 2). By default \code{t.degree=2} }
  \item{robust}{ if \code{TRUE} a robust regression method is used. Otherwise
    (\code{FALSE}), by default, a classical least-square regression is used }
  \item{trend}{ If \code{TRUE} a trend is calculated (under R only). Otherwise,
    the series is decomposed into a seasonal component and residuals only }
  \item{xreg}{ a second regular time series or a vector of the same length as
    \code{x} with corresponding values from the regression model }
  \item{object}{ a 'tsd' object as returned by the function \code{tsd()}, or any
    of the \code{decXXXX()} functions }
  \item{e}{ a 'tsd' object as returned by the function \code{tsd()}, or any of
    the \code{decXXXX()} functions }
  \item{series}{ (1) for \code{plot()}: the series to plot. By default,
    \code{series=1}, the first (or possibly unique) series in the 'tsd' object
    is plotted. (2) for \code{extract}: the name or the index of the series to
    extract. If \code{series} is provided, then \code{n} is ignored. By default,
    \code{series=NULL}. It is also possible to use negative indices. In this
    case, all series are extracted, except those ones }
  \item{stack}{ graphs of each component are either stacked (\code{stack=TRUE},
  by default), or superposed on the same graph \code{stack=FALSE} }
  \item{resid}{ do we have to plot also the "residuals" components
    (\code{resid=TRUE}, by default) or not? Usually, in a stacked graph, you
    would like to plot the residuals, while in a superposed graph, you would not }
  \item{col}{ color of the plot }
  \item{lty}{ line type for the plot }
  \item{labels}{ the labels to use for all y-axes in a stacked graph, or in the
    legend for a superposed graph. By default, the names of the components
    ("trend", "seasonal", "deseasoned", "filtered", "residuals", ...) are used }
  \item{leg}{ only used when \code{stack=FALSE}. Do we plot a legend
    (\code{leg=TRUE} or not? }
  \item{lpos}{ position of the upper-left corner of the legend box in the graph
    coordinates (x,y). By default, \code{leg=c(0,0)} }
  \item{xlab}{ the label of the x-axis }
  \item{ylab}{ the label of the y-axis }
  \item{main}{ the main title of the graph}
  \item{n}{ the number of series to extract (from series 1 to series n). By
    default, n equals the number of series in the 'tsd' object. If both
    \code{series} and \code{components} arguments are NULL, all series and
    components are extracted and this method has exactly the same effect as
    \code{tseries} }
  \item{components}{ the names or indices of the components to extract. If
    \code{components=NULL} (by default), then all components of the selected
    series are extracted. It is also possible to specify negative indices. In
    this case, all components are extracted, except those ones }
  \item{\dots}{ (1) for \code{tsd()}: further arguments to pass to the
  corresponding \code{decXXXX()} function. (2) for \code{plot()}: further
  graphical arguments, (3) unused for the other functions or methods }
}

\details{
  To eliminate trend from a series, use "diff" or use "loess" with
  \code{trend=TRUE}. If you know the shape of the trend (linear, exponential,
  periodic, etc.), you can also use it with the "reg" (regression) method. To
  eliminate or extract seasonal components, you can use "loess" if the seasonal
  component is additive, or "census" if it is multiplicative. You can also use
  "average" with argument \code{order="periodic"} and with either an additive or
  a multiplicative model, although the later method is often less powerful than
  "loess" or "census". If you want to extract a seasonal cycle with a given
  shape (for instance, a sinusoid), use the "reg" method with a fitted
  sinusoidal equation. If you want to identify levels in the series, use the
  "median" method. To smooth the series, you can use preferably the "evf"
  (eigenvector filtering), or the "average" methods, but you can also use
  "median". To extract most important components from the series (no matter if
  they are cycles -seasonal or not-, or long-term trends), you should use the
  "evf" method. For more information on each of these methods, see online help
  of the corresponding \code{decXXXX()} functions.
}
\value{
  An object of type 'tsd' is returned. It has methods \code{print()},
  \code{summary()}, \code{plot()}, \code{extract()} and \code{specs()}.
}
\references{ 
Kendall, M., 1976. \emph{Time-series.} Charles Griffin & Co Ltd. 197 pp.

Laloire, J.C., 1972. \emph{Méthodes du traitement des chroniques.} Dunod, Paris, 194 pp.

Legendre, L. & P. Legendre, 1984. \emph{Ecologie numérique. Tome 2: La structure
  des données écologiques.} Masson, Paris. 335 pp.

Malinvaud, E., 1978. \emph{Méthodes statistiques de l'économétrie.} Dunod, Paris. 846 pp.

Philips, L. & R. Blomme, 1973. \emph{Analyse chronologique.} Université
  Catholique de Louvain. Vander ed. 339 pp.
}

\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}),
  Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\note{ If you have to decompose a single time series, you could also use the
  corresponding \code{decXXXX()} function directly. In the case of a multivariate
  regular time series, \code{tsd()} is more convenient because it decompose all
  times series of a set at once! }

\seealso{ \code{\link{tseries}}, \code{\link{decdiff}}, \code{\link{decaverage}},
  \code{\link{decmedian}}, \code{\link{decevf}}, \code{\link{decreg}},
  \code{\link{decloess}}, \code{\link{deccensus}} }

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
