\name{trend.test}
\alias{trend.test}
\title{ Test if an increasing or decreasing trend exists in a time series }
\description{
Test if the series has an increasing or decreasing trend, using a non-parametric Spearman test between the observations and time
}
\usage{
trend.test(tseries, R=1)
}
\arguments{
  \item{tseries}{ a univariate or multivariate time series (a 'rts' object in Splus or a 'ts' object in \R) }
  \item{R}{ The number of time the series is/are resampled for a bootstrap test. If \code{R1} (by default), an usual Spearman test is performed. If \code{R} > 1 then a bootstrap test is run }
}
\value{
A 'htest' object if \code{R=1}, a 'boot' object with an added \code{boot$p.value} item otherwise 
}
\references{ 
Siegel, S. & N.J. Castellan, 1988. \emph{Non-parametric statistics.} McGraw-Hill, New York. 399  pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\note{ 
In both cases (normal test with \code{R=1} and bootstrap test), the p-value can be obtained from \code{obj$p.value} (see examples)
}

\seealso{ \code{\link{local.trend}} }

\examples{
data(marbio)
trend.test(marbio[, 8])
# Run a bootstrap test on the same series
marbio8.trend.test <- trend.test(marbio[, 8], R=99)
# R=999 is a better value... but it is very slow!
marbio8.trend.test  
plot(marbio8.trend.test)
marbio8.trend.test$p.value
}
\keyword{ ts }
\keyword{ htest }
