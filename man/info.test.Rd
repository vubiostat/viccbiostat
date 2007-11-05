\name{info.test}
\alias{info.test}
\title{InfoScore}
\description{
  InfoScore method for high-dimensional data analysis.
}
\usage{
  info.test(x, y, \dots)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{numeric vector of data values.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\value{
  InfoScore statistic.
}
\references{
  citation needed
}
\examples{
  \dontshow{
    x <- 2*(1:101) - 1
    y <- 2*(1:100)
  }
  info.test(x, y)
}
\keyword{htest}
