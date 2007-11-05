\name{wga.test}
\alias{wga.test}
\title{Weighted Gene Analysis}
\description{
  Uses the NIH's Weighted Method to determine differentially expressed biomarkers.
}
\usage{
  wga.test(x, y, \dots)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{numeric vector of data values.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\value{
  WGA statistic.
}
\references{
  Ingrid Hedenfalk et at (2001) "Gene-Expression Profiles in Hereditary Breast Cancer", \emph{New England Journal of Medicine}, \bold{344}, 539-548.
}
\keyword{htest}
