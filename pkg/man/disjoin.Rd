\name{disjoin}
\alias{disjoin}

\encoding{latin1}

\title{ Complete disjoined coded data (binary coding) }
\description{
  Transform a factor in separate variables (one per level) with a binary code (0 for absent, 1 for present) in each variable
}
\usage{
disjoin(x)
}
\arguments{
  \item{x}{ a vector containing a factor data }
}
\details{
  Use \code{cut()} to transform a numerical variable into a factor variable
}
\value{
  a matrix containing the data with binary coding
}
\references{
Fromentin J.-M., F. Ibanez & P. Legendre, 1993. \emph{A phytosociological method for interpreting plankton data.} Mar. Ecol. Prog. Ser., 93:285-306.

Gebski, V.J., 1985. \emph{Some properties of splicing when applied to non-linear smoothers.} Comput. Stat. Data Anal., 3:151-157.

Grandjouan, G., 1982. \emph{Une méthode de comparaison statistique entre les répartitions des plantes et des climats.} Thèse d'Etat, Université Louis Pasteur, Strasbourg.

Ibanez, F., 1976. \emph{Contribution à l'analyse mathématique des événements en Ecologie planctonique. Optimisations méthodologiques.} Bull. Inst. Océanogr. Monaco, 72:1-96.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\seealso{ \code{\link{buysbal}}, \code{\link[base]{cut}} }

\examples{
# Artificial data with 1/5 of zeros
Z <- c(abs(rnorm(8000)), rep(0, 2000))
# Let the program chose cuts
table(cut(Z, breaks=5))
# Create one class for zeros, and 4 classes for the other observations
Z2 <- Z[Z != 0]
cuts <- c(-1e-10, 1e-10, quantile(Z2, 1:5/5, na.rm=TRUE))
cuts
table(cut(Z, breaks=cuts))
# Binary coding of these data
disjoin(cut(Z, breaks=cuts))[1:10, ]
}
\keyword{ manip }
