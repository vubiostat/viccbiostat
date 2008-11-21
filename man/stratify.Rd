\name{stratify}
\alias{stratify}
\title{Stratify Data}
\description{
  Uses the interaction of multiple factors to "evenly" distribute data into folds.
}
\usage{
stratify(..., n, RANDOMIZE = TRUE)
}
\arguments{
  \item{\dots}{unnamed arguments are factors to concatenate and named arguments are extra parameters for \code{interaction}.}
  \item{n}{numeric indicating the number of groups to divide the data into.}
  \item{RANDOMIZE}{logical indicating whether or not to use \code{sample} in distributing the data.}
}
\seealso{
  \code{\link{interaction}}
  \code{\link{sample}}
}
\keyword{utilities}
