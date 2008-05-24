\name{last}
\alias{last}
\title{ Get the last element of a vector }
\description{
  Extract the last element of a vector. Useful for the \code{turnogram()} function
}
\usage{
last(x, na.rm=FALSE)
}
\arguments{
  \item{x}{ a numerical vector }
  \item{na.rm}{ if \code{na.rm=TRUE}, then the last non-missing value (if any) is returned. By default, it is FALSE and the last element (whatever its value) is returned }
}
\value{
  a numerical value
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{first}}, \code{\link{turnogram}} }

\examples{
a <- c(NA, 1, 2, NA, 3, 4, NA)
last(a)
last(a, na.rm=TRUE)
}
\keyword{ manip }
