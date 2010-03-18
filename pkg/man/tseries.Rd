\name{tseries}
\alias{tseries}

\encoding{latin1}

\title{ Convert a 'regul' or a 'tsd'  object into a time series }
\description{
  Regulated series contained in a 'regul' object or components issued from a time series decomposition with 'tsd' are extracted from their respective object and converted into uni- or multivariate regular time series ('rts' objects in Splus and 'ts' objects in \R)
}
\usage{
tseries(x)
}
\arguments{
  \item{x}{ A 'regul' or 'tsd' object }
}
\value{
  an uni- or multivariate regular time series
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\note{
To extract some of the time series contained in the 'regul' or 'tsd' objects, use the \code{extract()} method
}

\seealso{ \code{\link{is.tseries}}, \code{\link{regul}}, \code{\link{tsd}} }

\examples{
data(releve)
rel.regy <- regul(releve$Day, releve[3:8], xmin=6, n=87, units="daystoyears",
        frequency=24, tol=2.2, methods="linear", datemin="21/03/1989",
        dateformat="d/m/Y")
# This object is not a time series
is.tseries(rel.regy)     # FALSE
# Extract all time series contained in the 'regul' object
rel.ts <- tseries(rel.regy)
# Now this is a time series
is.tseries(rel.ts)       # TRUE
}
\keyword{ ts }
\keyword{ manip }
