\name{decreg}
\alias{decreg}

\encoding{latin1}

\title{ Time series decomposition using a regression model }
\description{
  Providing values coming from a regression on the original series, a \code{tsd} object is created using the original series, the regression model and the residuals
}
\usage{
decreg(x, xreg, type="additive")
}
\arguments{
  \item{x}{ a regular time series ('rts' under S+ and 'ts' under R) }
  \item{xreg}{ a second regular time series or a vector of the same length as \code{x} with corresponding values from the regression model }
  \item{type}{ the type of model, either \code{type="additive"} (by default), or \code{type="multiplicative"} }
}
\value{
  a 'tsd' object
}
\references{ 
Frontier, S., 1981. \emph{Méthodes statistiques.} Masson, Paris. 246 pp.

Kendall, M., 1976. \emph{Time-series.} Charles Griffin & Co Ltd. 197 pp.

Legendre, L. & P. Legendre, 1984. \emph{Ecologie numérique. Tome 2: La structure des données écologiques.} Masson, Paris. 335 pp.

Malinvaud, E., 1978. \emph{Méthodes statistiques de l'économétrie.} Dunod, Paris. 846 pp.

Sokal, R.R. & F.J. Rohlf, 1981. \emph{Biometry.} Freeman & Co, San Francisco. 860 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{tsd}}, \code{\link{tseries}}, \code{\link{decaverage}}, \code{\link{deccensus}}, \code{\link{decdiff}}, \code{\link{decevf}}, \code{\link{decmedian}}, \code{\link{decloess}} }

\examples{
data(marphy)
density <- ts(marphy[, "Density"])
plot(density)
Time <- time(density)

# Linear model to represent trend
density.lin <- lm(density ~ Time)
summary(density.lin)
xreg <- predict(density.lin)
lines(xreg, col=3)
density.dec <- decreg(density, xreg)
plot(density.dec, col=c(1, 3, 2), xlab="stations")

# Order 2 polynomial to represent trend
density.poly <- lm(density ~ Time + I(Time^2))
summary(density.poly)
xreg2 <- predict(density.poly)
plot(density)
lines(xreg2, col=3)
density.dec2 <- decreg(density, xreg2)
plot(density.dec2, col=c(1, 3, 2), xlab="stations")

# Fit a sinusoidal model on seasonal (artificial) data
tser <- ts(sin((1:100)/12*pi)+rnorm(100, sd=0.3), start=c(1998, 4),
        frequency=24)
Time <- time(tser)
tser.sin <- lm(tser ~ I(cos(2*pi*Time)) + I(sin(2*pi*Time)))
summary(tser.sin)
tser.reg <- predict(tser.sin)
tser.dec <- decreg(tser, tser.reg)
plot(tser.dec, col=c(1, 4), xlab="stations", stack=FALSE, resid=FALSE,
        lpos=c(0, 4))
plot(tser.dec, col=c(1, 4, 2), xlab="stations")

# One can also use nonlinear models (see 'nls')
# or autoregressive models (see 'ar' and others in 'ts' library)
}
\keyword{ ts }
\keyword{ smooth }
