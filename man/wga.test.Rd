\name{wga.test}
\alias{wga.test}
\alias{wga.test.formula}
\alias{wga.test.default}
\title{Weighted Gene Analysis}
\description{
  Uses the NIH's Weighted Method to determine differentially expressed biomarkers.
}
\usage{
wga.test(x, \dots)

\method{wga.test}{default}(x, y, \dots)

\method{wga.test}{formula}(formula, data, subset, na.action, \dots)
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
  WGA statistic.
}
\references{
  Ingrid Hedenfalk et at (2001) "Gene-Expression Profiles in Hereditary Breast Cancer", \emph{New England Journal of Medicine}, \bold{344}, 539-548.
}
\keyword{htest}
