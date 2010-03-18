\name{stat.desc}
\alias{stat.desc}

\encoding{latin1}

\title{ Descriptive statistics on a data frame or time series }
\description{
  Compute a table giving various descriptive statistics about the series in a data frame or in a single/multiple time series
}
\usage{
stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
}
\arguments{
  \item{x}{ a data frame or a time series }
  \item{basic}{ do we have to return basic statistics (by default, it is TRUE)? These are: the number of values (nbr.val), the number of null values (nbr.null), the number of missing values (nbr.na), the minimal value (min), the maximal value (max), the range (range, that is, max-min) and the sum of all non-missing values (sum) }
  \item{desc}{ do we have to return various descriptive statistics (by default, it is TRUE)? These are: the median (median), the mean (mean), the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the \code{p} level, the variance (var), the standard deviation (std.dev) and the variation coefficient (coef.var) defined as the standard deviation divided by the mean }
  \item{norm}{ do we have to return normal distribution statistics (by default, it is FALSE)? the skewness coefficient g1 (skewness), its significant criterium (skew.2SE, that is, g1/2.SEg1; if skew.2SE > 1, then skewness is significantly different than zero), kurtosis coefficient g2 (kurtosis), its significant criterium (kurt.2SE, same remark than for skew.2SE), the statistic of a Shapiro-Wilk test of normality (normtest.W) and its associated probability (normtest.p) }
  \item{p}{ the probability level to use to calculate the confidence interval on the mean (CI.mean). By default, \code{p=0.95} }
}
\value{
  a data frame with the various statistics in rows and with each column correponding to a variable in the data frame, or to a separate time series
}
\author{ Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}), Philippe Grosjean (\email{phgrosjean@sciviews.org}) }
\note{ The Shapiro-Wilk test of normality is not available yet in Splus and it returns 'NA' in this environment. If you prefer to get separate statistics for various time intervals in your time series, use \code{stat.slide()}. If your data are fish or plankton sampled with a net, consider using the Pennington statistics (see \code{stat.pen()}) }

\seealso{ \code{\link{stat.slide}}, \code{\link{stat.pen}} }

\examples{
data(marbio)
stat.desc(marbio[,13:16], basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
}
\keyword{ ts }
