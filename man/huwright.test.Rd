\name{huwright.test}
\alias{huwright.test}
\alias{huwright.test.formula}
\alias{huwright.test.default}
\title{Hu-Wright Test}
\description{
  Class comparison for high-dimensional data.
}
\usage{
huwright.test(x, \dots)

\method{huwright.test}{default}(x, y, tol=1, \dots)

\method{huwright.test}{formula}(formula, subset, na.action, \dots)
}
\arguments{
  \item{x}{numeric matrix or data.frame of data values with all variables.}
  \item{y}{numeric matrix or data.frame of data values with all variables.}
  \item{tol}{tolerance(?).}
  \item{formula}{a formula of the form \code{y ~ grp}, where \code{y} is a matrix or data frame (or similar: see \code{\link{model.frame}}) giving the data values and \code{grp} a factor with two levels giving the corresponding groups.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
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
