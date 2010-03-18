\name{escouf}
\alias{escouf}
\alias{extract.escouf}
\alias{identify.escouf}
\alias{lines.escouf}
\alias{plot.escouf}
\alias{print.escouf}
\alias{print.summary.escouf}
\alias{summary.escouf}

\encoding{latin1}

\title{ Choose variables using the Escoufier's equivalent vectors method }
\description{
  Calculate equivalent vectors sensu Escoufier, that is, most significant variables from a multivariate data frame according to a principal component analysis (variables that are most correlated with the principal axes). This method is useful mainly for physical or chemical data where simply summarizing them with a PCA does not always gives easily interpretable principal axes.
}
\synopsis{
escouf(x, level=1, verbose=TRUE)
extract.escouf(e, n, level=e$level, ...)
identify.escouf(x, lhorz=TRUE, lvert=TRUE, lvars=TRUE, col=2, lty=2, ...)
lines.escouf(x, level=x$level, lhorz=TRUE, lvert=TRUE, lvars=TRUE, col=2, lty=2, ...)
plot.escouf(x, level=x$level, lhorz=TRUE, lvert=TRUE, lvars=TRUE, lcol=2, llty=2, diff=TRUE, dlab="RV' (units not shown)", dcol=4, dlty=par("lty"), dpos=0.8, type="s", xlab="variables", ylab="RV", main=paste("Escoufier's equivalent vectors for:",x$data), ...)
print.escouf(x, ...)
print.summary.escouf(x, ...)
summary.escouf(object, ...)  
}
\usage{
escouf(x, level=1, verbose=TRUE)
\method{summary}{escouf}(esc)
\method{plot}{escouf}(esc, level=x$level, lhorz=TRUE, lvert=TRUE, lvars=TRUE,
        lcol=2, llty=2, diff=TRUE, dlab="RV' (units not shown)", dcol=4,
        dlty=par("lty"), dpos=0.8, \dots)
\method{lines}{escouf}(esc, level=x$level, lhorz=TRUE, lvert=TRUE, lvars=TRUE,
        lcol=2, llty=2, \dots)
\method{identify}{escouf}(esc, lhorz=TRUE, lvert=TRUE, lvars=TRUE, lcol=2,
        llty=2, \dots)
\method{extract}{escouf}(esc, n=NULL, level=e$level)
}
\arguments{
  \item{x}{ A data frame containing the variables to sort according to the Escoufier's method }
  \item{level}{ The level of correlation at which to stop calculation. By default \code{level=1}, the highest value, and all variables are sorted. Specify a value lower than one to speed up calculation. If you specify a too low values you will not be able to extract all significant variables (extraction level must be lower than calculation level). We advise you keep 0.95 < level < 1 }
  \item{verbose}{ Print calculation steps. This allows to control the percentage of calculation already achieved when computation takes a long time (that is, with many variables to sort) }
  \item{esc}{ An 'escouf' object returned by \code{escouf}}
  \item{lhorz}{ If \code{TRUE} then an horizontal line indicating the extraction level is drawn }
  \item{lvert}{ If \code{TRUE} then a vertical line separate the n extracted variables at left from the rest }
  \item{lvars}{ If \code{TRUE} then the x-axis labels of the n extracted variables at left are printed in a different color to emphasize them }
  \item{lcol}{ The color to use to draw the lines (\code{lhorz=TRUE} and \code{lvert=TRUE}) and the variables labels (\code{lvars=TRUE}) of the n extracted variables. By default, color 2 is used }
  \item{llty}{ The style used to draw the lines (\code{lhorz=TRUE} and \code{lvert=TRUE}). By default, lines are dashed }
  \item{diff}{ If \code{TRUE} then the RV' curve is also plotted (by default) }
  \item{dlab}{ The label to use for the RV' curve. By default: \code{"RV' (units not shown)"} }
  \item{dcol}{ The color to use for the RV' curve (by default, color 4 is used) }
  \item{dlty}{ The style for the RV' curve }
  \item{dpos}{ The relative horizontal position of the label for the RV' curve. The default value of 0.8 means that the label is placed at 80\% of the horizontal axis.Vertical position of the label is automatically determined }
  \item{\dots}{ additional graph parameters }
  \item{n}{ The number of variables to extract. If a value is given, it has the priority on \code{level} }
}

\value{
  An object of type 'escouf' is returned. It has methods \code{print()}, \code{summary()}, \code{plot()}, \code{lines()}, \code{identify()}, \code{extract()}.
}
\references{ 
Cambon, J., 1974. \emph{Vecteur équivalent à un autre au sens des composantes principales.} Application hydrologique. DEA de Mathématiques Appliquées, Université de Montpellier.

Escoufier, Y., 1970. \emph{Echantillonnage dans une population de variables aléatoires réelles.} Pub. Inst. Stat. Univ. Paris, 19:1-47.

Jabaud, A., 1996. \emph{Cadre climatique et hydrobiologique du lac Léman.} DEA d'Océanologie Biologique Paris.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}), Benjamin Planque (\email{Benjamin.Planque@ifremer.fr}),  Jean-Marc Fromentin (\email{Jean.Marc.Fromentin@ifremer.fr}) }
\section{WARNING }{Since a large number of iterations is done, this function is slow with a large number of variables (more than 25-30)!}

\seealso{ \code{\link{abund}} }

\examples{
data(marbio)
marbio.esc <- escouf(marbio)
summary(marbio.esc)
plot(marbio.esc)
# The x-axis has short labels. For more info., enter: 
marbio.esc$vr
# Define a level at which to extract most significant variables
marbio.esc$level <- 0.90
# Show it on the graph
lines(marbio.esc)
# This can also be done interactively on the plot using:
# marbio.esc$level <- identify(marbio.esc)
# Finally, extract most significant variables
marbio2 <- extract(marbio.esc)
names(marbio2)
}
\keyword{ multivariate }
