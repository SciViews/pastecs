\name{first}
\alias{first}

\encoding{latin1}

\title{ Get the first element of a vector }
\description{
  Extract the first element of a vector. Useful for the \code{turnogram()} function
}
\usage{
first(x, na.rm=FALSE)
}
\arguments{
  \item{x}{ a numerical vector }
  \item{na.rm}{ if \code{na.rm=TRUE}, then the first non-missing value (if any) is returned. By default, it is FALSE and the first element (whatever its value) is returned }
}
\value{
  a numerical value
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{last}}, \code{\link{turnogram}} }

\examples{
a <- c(NA, 1, 2, NA, 3, 4, NA)
first(a)
first(a, na.rm=TRUE)
}
\keyword{ manip }
