\name{roc}
\alias{roc}
\alias{roc.formula}
\alias{roc.default}
\title{Receiver Operating Characteristic}
\description{
  Get the sensitivity and specificity at all possible cutoff points.
}
\usage{
roc(x, \dots)

\method{roc}{formula}(formula, data = NULL, \dots, subset, na.action = NULL)

\method{roc}{default}(x, \dots, group)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ grp}, where \code{y} is a numeric vector of data values to be split into groups according to the grouping variable \code{grp} (usually a factor).}
  \item{data}{a data.frame (or list) from which the variables in \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s.  The default is to ignore missing values in either the response or the group.}
  \item{x}{for specifying data from which the results are to be produced.  Either a numeric vector, or a single list containing such vectors.  Additional unnamed arguments specify further data as separate vectors (each corresponding to a group).}
  \item{\dots}{For the \code{formula} method, named arguments to be passed to the default method.
  
  For the default method, unnamed arguments are additional data vectors (unless \code{x} is a list or \code{group} is provided), and named arguments are extra parameters.}
  \item{group}{2-level factor of the classification information for \code{x}.}
}
\value{
  \item{sens}{Sensitivity cutoffs}
  \item{spec}{Specificity cutoffs}
  \item{auc}{Area under the curve}
}
\seealso{\link[ROC]{rocdemo.sca}}
\examples{
}
\keyword{models}
