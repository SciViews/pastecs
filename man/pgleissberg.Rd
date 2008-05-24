\name{pgleissberg}
\alias{pgleissberg}
\title{ Gleissberg distribution probability }
\description{
  The Gleissberg distribution gives the probability to have k extrema in a series of n observations. This distribution is used in the turnogram to determine if monotony indices are significant (see \code{turnogram()})
}
\usage{
pgleissberg(n, k, lower.tail=TRUE, two.tailed=FALSE)
}
\arguments{
  \item{n}{ the number of observations in the series }
  \item{k}{ the number of extrema in the series, as calculated by \code{turnpoints()} }
  \item{lower.tail}{ if \code{lower.tail=TRUE} (by default) and \code{two.tailed=FALSE}, the left-side probability is returned. If it is FALSE, the right-side probability is returned }
  \item{two.tailed}{ if \code{two.tailed=TRUE}, the two-sided probability is returned. By default, it is FALSE and a one-sided probability is returned (left or right, depending on the value of \code{lower.tail} }
}
\value{
  a value giving the probability to have \code{k} extrema in a series of \code{n} observations
}
\references{ 
Dallot, S. & M. Etienne, 1990. \emph{Une méthode non paramétrique d'analyse des séries en océanographie biologique: les tournogrammes.} Biométrie et océanographie - Société de biométrie, 6, Lille, 26-28 mai 1986. IFREMER, Actes de colloques, 10:13-31.

Johnson, N.L. & Kotz, S., 1969. \emph{Discrete distributions.} J. Wiley & sons, New York, 328 pp.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ The Gleissberg distribution is asymptotically normal. For \code{n} > 50, the distribution is approximated by a Gaussian curve. For lower \code{n} values, the exact probability is returned (using data in the variable \code{.gleissberg.table} }

\seealso{ \code{\link{.gleissberg.table}}, \code{\link{turnpoints}}, \code{\link{turnogram}}}

\examples{
# Until n=50, the exact probability is returned
pgleissberg(20, 10, lower.tail=TRUE, two.tailed=FALSE)
# For higher n values, it is approximated by a normal distribution
pgleissberg(60, 33, lower.tail=TRUE, two.tailed=FALSE)
}
\keyword{ distribution }
