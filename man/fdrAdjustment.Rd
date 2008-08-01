\name{fdrAdjustment}
\alias{fdrAdjustment}
\title{FDR Adjustment for p-values}
\description{
  Adjust p-values for false discovery rate.
}
\usage{
fdrAdjustment(data, ranks = NULL, method = c("step.up", "step.down"), c = NULL)
}
\arguments{
  \item{data}{numeric vector of p-values to adjust.}
  \item{ranks}{numeric vector of the ranks associated with \code{data}.}
  \item{method}{character string specifying the type of adjustment, must be one of \code{"step.up"} (default) or \code{"step.down"}.}
  \item{c}{numeric of length 1.  Represents c(V), which often used as 1.  Defaults to \code{log(length(data)) - digamma(1)}.}
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
