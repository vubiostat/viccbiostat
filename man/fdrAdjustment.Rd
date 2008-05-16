\name{fdrAdjustment}
\alias{fdrAdjustment}
\title{FDR Adjustment for p-values}
\description{
  Adjust p-values for false discovery rate.
}
\usage{
fdrAdjustment(data, ranks = NULL, method = c("step.up", "step.down"))
}
\arguments{
  \item{data}{a numeric vector of p-values to adjust.}
  \item{ranks}{a numeric vector of the ranks associated with \code{data}.}
  \item{method}{a character string specifying the type of adjustment, must be one of \code{"step.up"} (default) or \code{"step.down"}.}
}
\details{
If \code{ranks} is \code{NULL}, the ranks are computed.  Otherwise, \code{ranks} should be the same length as \code{data}.
}
\value{
A \code{data.frame} with two columns: the ranks of \code{data} and the FDR-adjusted p-values.
}
\examples{
(p <- rnorm(10))
fdrAdjustment(p)
}
\keyword{utilities}
