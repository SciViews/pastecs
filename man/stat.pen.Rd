\name{stat.pen}
\alias{stat.pen}

\encoding{latin1}

\title{ Pennington statistics on a data frame or time series }
\description{
  Compute a table giving various descriptive statistics, including Pennington's estimators of the mean, the variance and the variance of the mean, about the series in a data frame or in a single/multiple time series
}
\usage{
stat.pen(x, basic=FALSE, desc=FALSE)
}
\arguments{
  \item{x}{ a data frame or a time series }
  \item{basic}{ do we have to return also basic statistics (by default, it is FALSE)? These are: the number of values (nbr.val), the number of null values (nbr.null), the number of missing values (nbr.na), the minimal value (min), the maximal value (max), the range (range, that is, max-min) and the sum of all non-missing values (sum) }
  \item{desc}{ do we have to return also various descriptive statistics (by default, it is FALSE)? These are: the median (median), the mean (mean), the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the \code{p} level, the variance (var), the standard deviation (std.dev) and the variation coefficient (coef.var) defined as the standard deviation divided by the mean }
}
\value{
  a data frame with the various statistics in rows and with each column correponding to a variable in the data frame, or to a separate time series
}
\references{ Aitchison, J., 1955. \emph{On the distribution of a positive random variable having a discrete probability mass at the origin.} J. Amer. Stat. Ass., 50:901-908.

Pennington, M., 1983. \emph{Efficient estimations of abundance for fish and plankton surveys.} Biometrics, 39:281-286.
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ 
If you prefer to get separate statistics for various time intervals in your time series, use \code{stat.slide()}. Various other descriptive statistics, including test of the normal distribution are also available in \code{stat.desc()}
}

\seealso{ \code{\link{stat.slide}}, \code{\link{stat.desc}} }

\examples{
data(marbio)
stat.pen(marbio[,c(4, 14:16)], basic=TRUE, desc=TRUE)
}
\keyword{ ts }
