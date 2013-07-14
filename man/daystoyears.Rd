\name{daystoyears}
\alias{daystoyears}
\alias{yearstodays}

\encoding{latin1}

\title{ Convert time units from "days" to "years" or back }
\description{
  Convert time scales. The time scale "days" corresponds to 1 unit per day. The time scale "years" uses 1 unit for 1 year. It is used in any analysis that requires seasonal decomposition and/or elimination. 
}

\usage{
daystoyears(x, datemin=NULL, dateformat="m/d/Y")
yearstodays(x, xmin=NULL)
}
\arguments{
  \item{x}{ A vector of time values }
  \item{datemin}{ A character string representing the first date, using a format corresponding to \code{dateformat}. For instance, with \code{datemin="04/23/1998"} and \code{dateformat="m/d/Y"}, the first observation is assumed to be made the 23th April 1998. In \R, it can also be a POSIXt date (see \code{?DataTimeClasses}). In this case, \code{dateformat} is not required and is ignored. By default, \code{datemin=NULL} }
  \item{dateformat}{ The format used for the date in \code{datemin}. For instance, \code{"d/m/Y"} or \code{"m/d/y"}. The distinction between "Y" and "y" is not important in Splus, but it is vital in \R to use "y" for two-digit years (ex: 89) and "Y" for four-digits years (ex: 1989), or the date will not be correctly converted! In \R, you can also use a POSIXt format specification like "\%d-\%m\%Y" for instance (see \code{?strptime} for a complete description of POSIXt format specification. In both Splus and \R, you can also use "mon" for abbreviated months like "mon d Y" for "Apr 20 1965", and "month" for fully-spelled months like "d month Y" for "24 September 2003" }
  \item{xmin}{ The minimum value for the "days" time-scale }
}
\details{
  The "years" time-scale uses one unit for each year. We deliberately "linearized" time in this time-scale and each year is considered to have exactly 365.25 days. There is thus no adjustment for lep years.
  Indeed, a small shift (less than one day) is introduced. This could result, for some dates, especially the 31st December, or 1st January of a year to be considered as belonging to the next, or previous year, respectively!
  Similarly, one month is considered to be 1/12 year, no mather if it has 28, 29, 30 or 31 days. Thus, the same warning applies: there are shifts in months introduced by this linearization of time!
  This representation simplifies further calculations, especially regarding seasonal effects (a quarter is exactly 0.25 units for instance), but shifts introduced in time may or may not be a problem for your particular application
  (if exact dates matters, do not use this; if shifts of up to one day is not significant, there is no problem, like when working on long-term biological series with years as units).
  Notice that converting it back to "days", using \code{yearstodays()} restablishes exact dates without errors. So, no data is lost, it just a conversion to a simplified (linearized) calendar!
}
\value{
  A numerical vector of the same length as \code{x} with the converted time-scale
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{buysbal}} }

\examples{
# A vector with a "days" time-scale (25 values every 30 days)
A <- (1:25)*30
# Convert it to a "years" time-scale, using 23/05/2001 (d/m/Y) as first value
B <- daystoyears(A, datemin="23/05/2001", dateformat="d/m/Y")
B
# Convert it back to "days" time-scale
yearstodays(B, xmin=30)

# Here is an example showing the shift introduced, and its consequence:
C <- daystoyears(unclass(as.Date(c("1970-1-1","1971-1-1","1972-1-1","1973-1-1"),
	format = "\%Y-\%m-\%d")))
C
}
\keyword{ ts }
