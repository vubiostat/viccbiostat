\name{huwright.test}
\alias{huwright.test}
\title{Hu-Wright Test}
\description{
  Class comparison for high-dimensional data.
}
\usage{
  huwright.test(x, y, tol=1, \dots)
}
\arguments{
  \item{x}{numeric matrix or data.frame of data values with all variables.}
  \item{y}{numeric matrix or data.frame of data values with all variables.}
  \item{tol}{tolerance(?).}
  \item{\dots}{further arguments to be pass to or from methods.}
}
\details{
  Data should be in a sample x variable format and split into 2 classes.
}
\value{
  Vector of statistics for each variable in the data.
}
\references{
  Hu, J., Zou, F., and Wright, F. A. (2005) "Practical FDR-based sample size calculations in microarray experiments", \emph{Bioinformatics}, \bold{21}, 3264-3272.
}
\keyword{htest}
