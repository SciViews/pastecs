\name{marphy}
\alias{marphy}

\encoding{latin1}

\non_function{}
\title{ Physico-chemical records at the same stations as for marbio }
\usage{data(marphy)}
\description{
The \code{marphy} data frame has 68 rows (stations) and 4 columns.
They are seawater measurements at a deep of 3 to 7 m at the same 68 stations as \code{marbio}.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Temperature}{Seawater temperature in °C}
    \item{Salinity}{Salinity in g/kg}
    \item{Fluorescence}{Fluorescence of chlorophyll a}
    \item{Density}{Excess of volumic mass of the seawater in g/l}
  }
}
\details{
This dataset corresponds to a single transect sampled across the Ligurian Sea front in the Mediterranean Sea, between Nice (France) and Calvi (Corsica). The transect extends from the Cap Ferrat (close to Nice) to about 65 km offshore in the direction of Calvi (along a bearing of 123°). 68 regularly spaced measurements where recorded. They correspond to the stations where zooplankton was collected in the \code{marbio} dataset. Water masses in the transect across the front where identified as:
\describe{
    \item{Stations 1 to 17}{Peripheral zone}
    \item{Stations 17 to 25}{D1 (divergence) zone}
    \item{Stations 25 to 30}{C (convergence) zone}
    \item{Stations 30 to 41}{Frontal zone}
    \item{Stations 41 to 46}{D2 (divergence) zone}
    \item{Stations 46 to 68}{Central zone}
  }
}
\source{
Boucher, J., F. Ibanez & L. Prieur (1987) \emph{Daily and seasonal variations in the spatial distribution of zooplankton populations in relation to the physical structure in the Ligurian Sea Front.} Journal of Marine Research, 45:133-173.
}
\references{
Fromentin, J.-M., F. Ibanez & P. Legendre (1993) \emph{A phytosociological method for interpreting plankton data.} Marine Ecology Progress Series, 93:285-306.
}

\seealso{ \code{\link{marbio}} }

\keyword{datasets}
