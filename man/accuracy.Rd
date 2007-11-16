\name{accuracy}
\alias{accuracy}
\title{Accuracy, Sensitivity, Specificity}
\description{
  Calculate the overall and marginal accuracies from the known and predicted classifications.
}
\usage{
accuracy(known, predict)
}
\arguments{
  \item{known}{array or factor of known classifications.}
  \item{predict}{array or factor of predicted classifications.}
}
\details{
  Calculates the overall and marginal accuracies from the known and predicted classifications.  The margins are the union of the levels for the known and predicted classifications.
}
\value{
  Array of overall and marginal accuracies.  Each accuracy is named for its margin's level or overall.
}
\author{
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\examples{
  \dontshow{
    known <- c(rep(1:2,each=4))
    predict <- c(1, 2, 1, 1, 2, 1, 2, 1)
  }
  accuracy(known, predict)
}
\keyword{utilities}
