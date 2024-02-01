\name{GetUnitText}
\alias{GetUnitText}

\encoding{latin1}

\title{ Format a nice time units for labels in graphs }
\description{
  This is an internal function called by some \code{plot()} methods. Considering the time series 'units' attribute and the frequency of the observations in the series, the function returns a string with a pertinent time unit. For instance, if the unit is 'years' and the frequency is 12, then data are monthly sampled and \code{GetUnitText()} returns the string "months"
}
\usage{
GetUnitText(series)
}
\arguments{
  \item{series}{ a regular time series (a 'rts' object in Splus, or a 'ts' object in \R) }
}
\value{
  a string with the best time unit for graphs
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Fr?d?ric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{daystoyears}}, \code{\link{yearstodays}} }

\examples{
timeser <- ts(1:24, frequency=12)           # 12 observations per year
attr(timeser, "units") <- "years"           # time in years for 'ts' object
GetUnitText(timeser)                        # formats unit (1/12 year=months)
}
\keyword{ ts }
