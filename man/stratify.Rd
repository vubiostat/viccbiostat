\name{stratify}
\alias{stratify}
\title{Stratify Data}
\description{
  Uses the interaction of multiple factors to "evenly" distribute data into folds.
}
\usage{
stratify(..., n, RANDOMIZE = TRUE, drop = FALSE, sep = ".", lex.order = FALSE)
}
\arguments{
  \item{\dots}{unnamed arguments are factors to concatenate and named arguments are extra parameters for \code{interaction}.}
  \item{n}{numeric indicating the number of groups to divide the data into.}
  \item{RANDOMIZE}{logical indicating whether or not to use \code{sample} in distributing the data.}
  \item{drop}{if \code{drop} is \code{TRUE}, unused factor levels are dropped from the result.  The default is to retain all factor levels.}
  \item{sep}{string to construct the new level labels by joining the constituent ones.}
  \item{lex.order}{logical indicating if the order of factor concatenation should be lexically ordered.}
}
\seealso{
  \code{\link{interaction}}
  \code{\link{sample}}
}
\keyword{utilities}
