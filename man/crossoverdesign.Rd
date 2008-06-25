\name{crossoverdesign}
\alias{crossoverdesign}
\title{Crossover Design}
\description{
  Generate a crossover design randomized trial.
}
\usage{
crossoverdesign(s, t, permutations = FALSE)
}
\arguments{
  \item{s}{numeric.  Number of subjects in the trial.}
  \item{t}{numeric.  Number of treatments in the trial.}
  \item{permutations}{boolean.  Use full set of permutations or not.  When \code{t > 10}, this is ignored.}
}
\value{
  Returns a matrix with \code{s} rows and \code{t} columns.  Each entry is the treatment for the subject (row) in each round (column).
}
\examples{
crossoverdesign(10, 2)
}
\keyword{design}
