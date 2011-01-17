\name{dsp}
\alias{dsp}
\alias{dsp.formula}
\alias{dsp.default}
\title{Discrete Scatter Plot}
\description{
  Scatter plots of discrete bivariate data.  An alternative to sunflower plots.
}
\usage{
dsp(x, y, \dots)

\method{dsp}{formula}(formula, data = parent.frame(), \dots, subset)

\method{dsp}{default}(x, y, bkgr = TRUE, col = 1, pch = 19, cex = 0.8, \dots)
}
\arguments{
  \item{formula}{a formula, such as \code{x ~ y}.}
  \item{data}{a data.frame (or list) from which the variables in 'formula' should be taken.}
  \item{subset}{an optional vector specifying a subset of observations to be used in the fitting process.}
  \item{x}{the coordinates of points in the plot.}
  \item{y}{the y coordiates of points in the plot.}
  \item{bkgr}{a logical value indicating whether ....}
  \item{col}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.}
  \item{pch}{character or integer code for kind of points, see \code{\link{points.default}}.}
  \item{cex}{numerical value giving the amount by which plotting text/symbols should be magnified.}
  \item{\dots}{For the \code{formula} method, named arguments to be passed to the default method.

  For the default method, additional graphical parameters (see \code{\link{par}}).}
}
\details{
  The arguments \code{col}, \code{pch}, and \code{cex} may be vectors and will be recycled.
}
\value{
  A \code{\link{table}} of the plotted data.
}
\author{
  Tatsuki Koyama\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{tatsuki.koyama@vanderbilt.edu}\cr
  \cr
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\examples{
\dontshow{
set.seed(6)
x <- round(rnorm(400, 100, 4))
y <- round(rnorm(400, 200, 4))
sex <- sample(c('Female','Male'), 400, T)
}
dsp(y ~ x, pch=19, col=1+(sex=='Female'), cex=.6, bkgr=T, xlab='measurement 1', ylab='measurement 2', bty='L')
\dontshow{
legend( 'bottomright', pch=19, col=1:2, legend=c('Male','Female'), cex=.8 )
}
}
\keyword{hplot}
