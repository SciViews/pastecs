\name{specs}
\alias{specs}

\encoding{latin1}

\title{ Collect parameters ("specifications") from one object to use them in another analysis }
\description{
  `specs' is a generic function for reusing specifications included in an object and applying them in another similar analysis 
}
\usage{
specs(x, ...)
}
\arguments{
  \item{x}{ An object that has "specs" data }
  \item{\dots}{ Additional arguments (redefinition of one or several parameters) }
}
\value{
  A `specs' object that has the \code{print} method and that can be entered as an argument to functions using similar "specifications"
}
\seealso{ \code{\link{regul}}, \code{\link{tsd}} }

\keyword{ methods }
