\name{wilcoxon.test}
\alias{wilcoxon.test}
\alias{wilcoxon.test.formula}
\alias{wilcoxon.test.default}
\title{Wilcoxon Rank-Sum Test Wrapper}
\description{
  Wrapper for \code{wilcox.test} that adds the z value.
}
\usage{
wilcoxon.test(x, \dots)

\method{wilcoxon.test}{default}(x, y, \dots)

\method{wilcoxon.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{numeric vector of data values.}
  \item{formula}{a formula of the form \code{y ~ grp}, where \code{y} is a numeric variable giving the data values and \code{grp} a factor with two levels giving the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.  By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\value{
  A list with class \code{"htest"} containing the following extra components:

  \item{z}{approximation value.}
}
\seealso{
\code{\link{wilcox.test}}
}
\keyword{htest}
