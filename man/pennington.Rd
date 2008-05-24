\name{pennington}
\alias{pennington}
\title{ Calculate Pennington statistics }
\description{
  Calculate the mean, the variance and the variance of the mean for a single series, according to Pennington (correction for zero/missing values for abundance of species collected with a net)
}
\usage{
pennington(x, calc="all", na.rm=FALSE)
}
\arguments{
  \item{x}{ a numerical vector }
  \item{calc}{ indicate which estimator(s) should be calculated. Use: \code{"mean"}, \code{"var"}, \code{"mean.var"} or \code{"all"} (by default) for the mean, the variance, the variance of the mean or all these three statitics, respectively }
  \item{na.rm}{ if \code{na.rm=TRUE}, missing data are eliminated first. If it is FALSE (by default), any missing value in the series leads to NA as the result for the statistic }
}
\details{
  A complex problem in marine ecology is the notion of zero. In fact, the random sampling of a fish population often leads to a table with many null values. Do these null values indicate that the fish was absent or do we have to consider these null values as missing data, that is, that the fish was rare but present and was not caught by the net? For instance, for 100 net trails giving 80 null values, how to estimate the mean? Do we have to divide the sum of fishes caught by 100 or by 20?
  
  Pennington (1983) applied in this case the probabilistic theory of Aitchison (1955). The later established a method to estimate mean and variance of a random variable with a non-null probability to present zero values and whose the conditional distribution corresponding to its non-null values follows a Gaussian curve. In practice, a lognormal distribution is found most of the time for non-null values in fishes population. It is also often the case for the plankton, after our own experience. \code{pennington()} calculates thus statistics for such conditional lognormal distributions.
}
\value{
    a single value, or a vector with "mean", "var" and "mean.var" if the argument \code{calc="all"} 
}
\references{ 
Aitchison, J., 1955. \emph{On the distribution of a positive random variable having a discrete probability mass at the origin.} J. Amer. Stat. Ass., 50:901-908.

Pennington, M., 1983. \emph{Efficient estimations of abundance for fish and plankton surveys.} Biometrics, 39:281-286.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }

\note{
For multiple series, or for getting more statistics on the series, use \code{stat.pen()}. Use \code{stat.slide()} for obtaining statistics calculated separately for various intervals along the time for a time series
}

\seealso{ \code{\link{stat.pen}}, \code{\link{stat.slide}} }

\examples{
data(marbio)
pennington(marbio[, "Copepodits2"])
pennington(marbio[, "Copepodits2"], calc="mean", na.rm=TRUE)
}
\keyword{ univar }
