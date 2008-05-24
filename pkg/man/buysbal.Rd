\name{buysbal}
\alias{buysbal}
\title{ Buys-Ballot table }
\description{
  Calculate a Buys-Ballot table from a time-series
}
\usage{
buysbal(x, y=NULL, frequency=12, units="years", datemin=NULL,
        dateformat="m/d/Y", count=FALSE)
}
\arguments{
  \item{x}{ Either a vector with time values (in this case, \code{y} must be defined), or a regular time-series }
  \item{y}{ If \code{x} is a vector with time values, \code{y} must contain corresponding observations }
  \item{frequency}{ The frequency of observations per year to use in the Buys-Ballot table. By default \code{frequency=12} which corresponds to monthly samples, and the resulting table has 12 column, one per month }
  \item{units}{ either \code{"years"} (by default), and time is not rescaled, or \code{"days"}, and the time is rescaled to "years" with the function \code{daystoyears()} }
  \item{datemin}{ A character string representing the first date, using a format corresponding to \code{dateformat}. For instance, with \code{datemin="04/23/1998"} and \code{dateformat="m/d/Y"}, the first observation is assumed to be made the 23th April 1998. In \R, it can also be a POSIXt date (see \code{?DataTimeClasses}). In this case, \code{dateformat} is not required and is ignored. By default, \code{datemin=NULL} }
  \item{dateformat}{ The format used for the date in \code{datemin}. For instance, \code{"d/m/Y"} or \code{"m/d/Y"} (by default). The distinction between "Y" and "y" is not important in Splus, but it is vital in \R to use "y" for two-digit years (ex: 89) and "Y" for four-digits years (ex: 1989), or the date will not be correctly converted! In \R, you can also use a POSIXt format specification like "\%d-\%m\%Y" for instance (see \code{?strptime} for a complete description of POSIXt format specification. In both Splus and \R, you can also use "mon" for abbreviated months like "mon d Y" for "Apr 20 1965", and "month" for fully-spelled months like "d month Y" for "24 September 2003" }
  \item{count}{ If \code{FALSE} (by default), the Buys-Ballot table is calculated. If \code{TRUE}, the function returns only the number of observations that are used in each cell of the Buys-Ballot table }
}
\details{
  A Buys-Ballot table summarizes data to highlight seasonal variations. Each line is one year and each column is a period of the year (12 months, 4 quarters, 52 weeks,...). A cell ij of this table contain the mean value for all observations made during the year i at the period j.
}
\value{
  A matrix containing either the Buys-Ballot table (\code{count=FALSE}), or the number of observations used to build the table (\code{count=TRUE})
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{daystoyears}}, \code{\link{tsd}} }

\examples{
data(releve)
buysbal(releve$Day, releve$Melosul, frequency=4, units="days",
        datemin="21/03/1989", dateformat="d/m/Y")
buysbal(releve$Day, releve$Melosul, frequency=4, units="days",
        datemin="21/03/1989", dateformat="d/m/Y", count=TRUE)
}
\keyword{ ts }
