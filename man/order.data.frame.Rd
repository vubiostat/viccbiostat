\name{order.data.frame}
\alias{order.data.frame}
\title{Ordering Permutation}
\description{
  Returns a permutation which rearranges the first data.frame variable into ascending or descending order, breaking ties by further variables.
}
\usage{
order.data.frame(x, na.last = TRUE, decreasing = FALSE)
}
\arguments{
  \item{x}{the data.frame}
  \item{na.last}{logical vector for controlling the treatment of \code{NA}s.  If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed.}
  \item{decreasing}{logical vector for controlling the sort order.}
}
\details{
  The example for \code{\link{order}} shows how to sort a \code{\link{data.frame}} on multiple variables using \code{\link{with}}, but you cannot specify \code{na.last} and \code{decreasing} for each.

  In the case of ties in the first data.frame variable, values in the second are used to break them.  If the values are still tied, values in the later variables are used to break the tie.
}
\seealso{
  \code{\link{order}}
}
\keyword{manip}
\keyword{utilities}
