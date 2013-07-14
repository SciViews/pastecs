\name{abund}
\alias{abund}
\alias{extract.abund}
\alias{identify.abund}
\alias{lines.abund}
\alias{plot.abund}
\alias{print.abund}
\alias{print.summary.abund}
\alias{summary.abund}

\encoding{latin1}

\title{ Sort variables by abundance }

\description{
  Sort variables (usually species in a species x stations matrix) in function of
  their abundance, either in number of non-null values, or in number of
  individuals (in log). The \code{f} coefficient allows adjusting weight given to each of these two criteria.
}

\usage{
abund(x, f = 0.2)

\method{extract}{abund}(e, n, left = TRUE, ...)
\method{identify}{abund}(x, label.pts = FALSE, lvert = TRUE, lvars = TRUE, col = 2, lty = 2, ...)
\method{lines}{abund}(x, n = x$n, lvert = TRUE, lvars = TRUE, col = 2, lty = 2, ...)
\method{plot}{abund}(x, n = x$n, lvert = TRUE, lvars = TRUE, lcol = 2, llty = 2, all = TRUE,
    dlab = c("cumsum", "\% log(ind.)", "\% non-zero"), dcol = c(1,2,4),
    dlty = c(par("lty"), par("lty"), par("lty")), dpos = c(1.5, 20), type = "l",
    xlab = "variables", ylab = "abundance",
    main = paste("Abundance sorting for:",x$data, "with f =", round(x$f, 4)), ...)
\method{print}{abund}(x, ...)
\method{print}{summary.abund}(x, ...)
\method{summary}{abund}(object, ...)
}

\arguments{
  \item{x}{ A data frame containing the variables to sort according to their
    abundance in columns for \code{abund}, or an 'abund' object for the methods }
  \item{f}{ Weight given to the number of individuals criterium (strictly
    included between 0 and 1; weight for the non-null values is \code{1-f}. The
    default value, \code{f=0.2}, gives enough weight to the number of non-null
    values to get abundant species according to this criterium first, but
    allowing to get at the other extreme rare, but locally abundant species }
  \item{object}{ An 'abund' object returned by \code{abund} }
  \item{e}{ An 'abund' object returned by \code{abund} }
  \item{n}{ The number of variables selected at left }
  \item{type}{ the type of graph to plot. By default, lines with 'l' }
  \item{lvert}{ If \code{TRUE} then a vertical line separate the n variables at
    left from the others }
  \item{lvars}{ If \code{TRUE} then the x-axis labels of the n left variables
    are printed in a different color to emphasize them }
  \item{lcol}{ The color to use to draw the vertical line (\code{lvert=TRUE})
    and the variables labels (\code{lvars=TRUE}) at left af the nth variable.
    By default, color 2 is used }
  \item{llty}{ The style used to draw the vertical line (\code{lvert=TRUE}).
    By default, a dashed line is used }
  \item{xlab}{ the label of the x-axis }
  \item{ylab}{ the label of the y-axis }
  \item{main}{ the main title of the graph}
  \item{all}{ If \code{TRUE} then all lines are drawn (cumsum, \%log(ind.) and
    \%non-null). If \code{FALSE}, only the cumsum line is drawn }
  \item{dlab}{ The legend labels }
  \item{dcol}{ Colors to use for drawing the various curves on the graph }
  \item{dlty}{ The line style to use for drawing the various curves on the graph }
  \item{dpos}{ The position of the legend box on the graph (coordinates of its
    top-left corner). A legend box is drawn only if \code{all=TRUE} }
  \item{col}{ The color to use to draw lines }
  \item{lty}{ The style used to draw lines }
  \item{\dots}{ additional parameters }
  \item{label.pts}{ Do we have to label points on the graph or to chose an
    extraction level with the \code{identify()} method? }
  \item{left}{ If \code{TRUE}, the n variables at left are extracted. Otherwise,
    the total-n variables at right are extracted }
}
\details{
  Successive sorts can be applied. For instance, a first sort with
  \code{f = 0.2}, followed by an extraction of rare species and another sort
  with \code{f = 1} allows to collect only rare but locally abundant species.
}
\value{
  An object of type 'abund' is returned. It has methods \code{print()},
  \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{extract()}.
}
\references{
  Ibanez, F., J.-C. Dauvin & M. Etienne, 1993. \emph{Comparaison des évolutions
  à long terme (1977-1990) de deux peuplements macrobenthiques de la baie de
  Morlaix (Manche occidentale): relations avec les facteurs hydroclimatiques.}
  J. Exp. Mar. Biol. Ecol., 169:181-214.
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}),
  Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{escouf}} }

\examples{
data(bnr)
bnr.abd <- abund(bnr)
summary(bnr.abd)
plot(bnr.abd, dpos=c(105, 100))
bnr.abd$n <- 26
# To identify a point on the graph, use: bnr.abd$n <- identify(bnr.abd)
lines(bnr.abd)
bnr2 <- extract(bnr.abd)
names(bnr2)
}

\keyword{ multivariate }
