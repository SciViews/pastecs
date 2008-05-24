\name{is.tseries}
\alias{is.tseries}
\title{ Is this object a time series? }
\description{
  This is equivalent to \code{is.rts()} in Splus and to \code{is.ts()} in \R. \code{is.tseries()} recognizes both 'rts' and 'ts' objects whatever the environment (Splus or R)
}
\usage{
is.tseries(x)
}
\arguments{
  \item{x}{ an object }
}
\value{
  a boolean value
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{tseries}} }

\examples{
tser <- ts(sin((1:100)/6*pi)+rnorm(100, sd=0.5), start=c(1998, 4), frequency=12)
is.tseries(tser)      # TRUE
not.a.ts <- c(1,2,3)
is.tseries(not.a.ts)  # FALSE
}
\keyword{ ts }
