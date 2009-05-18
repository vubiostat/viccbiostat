\name{QC}
\alias{QC}
\title{Quality Control}
\description{
  Run various quality control methods on a data set.
}
\usage{
QC(x, group = NULL, methods = c("pearson", "spearman", "kendall", "kappa", "icc", "cv"), cutoff = 0, fixedEffect = 1, randomEffect = NULL, silent = FALSE)
}
\arguments{
  \item{x}{numeric data.frame or matrix.}
  \item{group}{factor of group information for the records/rows in \code{x} with length equal to number of rows in \code{x}.  Optional.}
  \item{methods}{character vector of the methods to run.}
  \item{cutoff}{numeric of length 1.  Used for \code{method="kappa"}.}
  \item{fixedEffect}{terms part of a two-sided linear formula.  See \code{nlme::lme}'s fixed.}
  \item{randomEffect}{grouping structure.  See \code{nlme::lme}'s random.}
  \item{silent}{logical.  Whether to print error messages regarding missing packages.}
}
\details{
When \code{group} is provided, \code{x} is \code{split} and the methods are run within each group.  Otherwise, the methods are run across groups.  Note: \code{cv} is unaffected.

\code{method = "pearson"}, \code{method = "spearman"}, and \code{method = "kendall"} use \code{stats::cor}.

\code{method = "kappa"} uses \code{e1071::classAgreement}.

\code{method = "icc"} uses \code{nlme::lme}.

\code{method = "cv"} provides mean, standard deviation, and coefficient of variance of each sample.
}
\value{
A \code{data.frame} of quality control metrics.
}
\author{
  Joan Zhang\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{xueqiong.zhang@vanderbilt.edu}\cr
  \cr
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\seealso{
  \code{\link[nlme]{lme}}, \code{\link[e1071]{classAgreement}}, \code{\link[stats]{cor}}, \code{\link[stats]{sd}}, \code{\link{mean}}
}
\examples{
}
\keyword{utilities}
\keyword{univar}
\keyword{multivariate}
