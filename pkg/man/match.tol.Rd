\name{match.tol}
\alias{match.tol}
\title{ Determine matching observation with a tolerance in time-scale }
\description{
  Determine which observations in a regular time series match observation in an original irregular one, accepting a given tolerance window in the time-scale. This function is internally used for regulation (functions \code{regul()}, \code{regul.screen()} and \code{regul.adj()}
}
\usage{
match.tol(x, table, nomatch=NA, tol.type="both", tol=0)
}
\arguments{
  \item{x}{ a numerical vector containing seeked values (time-scale of the regular series) }
  \item{table}{ a numerical vector containing initial times to look for match in \code{x} }
  \item{nomatch}{ the symbol tu use to flag an entry where no match is found. By default, \code{nomatch=NA} }
  \item{tol.type}{ the type of adjustment to use for the time-tolerance: \code{"left"}, \code{"right"}, \code{"both"} (by default) or \code{"none"}. If \code{tol.type="left"}, corresponding \code{x} values are seeked in a window ]table-tol, table]. If \code{tol.type="right"}, they are seeked in the window [table, table+tol[. If \code{tol.type="both"}, then they are seeked in the window ]table-tol, table+tol]. If several observations are in this window, the closest one is used. Finally, if \code{tol.type="none"}, then the function returns the \code{nomatch} symbol for all entries }
  \item{tol}{ the tolerance in the time-scale to determine if a value in \code{x} matches a value in \code{table}. If \code{tol=0}, observations in each respective series must match exactly, otherwise observations in the regulated series are interpolated. \code{tol} must be a round fraction of the interval between observations in \code{x} (x[i+1] - x[i], (x[i+1] - x[i])/2, (x[i+1] - x[i])/3, etc...), and cannot be larger than it, otherwise, \code{tol} is automatically adjusted to the closest allowed value. By default, \code{tol=NULL}. This is equivalent to \code{tol=0}. Warning! }
}
\value{
  a vector of the same length of \code{x}, indicating the position of the matching observations in \code{table}
}
\author{ Philippe Grosjean (\email{phgrosjean@sciviews.org}), Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}) }

\seealso{ \code{\link{regul}}, \code{\link{regul.screen}}, \code{\link{regul.adj}} }

\examples{
X <- 1:5
Table <- c(1, 3.1, 3.8, 4.4, 5.1, 6)
match.tol(X, Table)
match.tol(X, Table, tol=0.2)
match.tol(X, Table, tol=0.55)
}
\keyword{ ts }
\keyword{ manip }
