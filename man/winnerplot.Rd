\name{winnerplot}
\alias{winnerplot}
\title{Winner Plot}
\description{
  Plot the average spectra for a specific range.
}
\usage{
  winnerplot(data, winner, legend = NULL, bin = NULL, xlim = NULL, pad = diff(bin), lty = par("lty"), lwd = par("lwd"), col = par("col"), add = FALSE, \dots)
}
\arguments{
  \item{data}{numeric matrix of average spectra data.  The first column should be the x coordinates (mass) and each subsequent column should be y coordinates.}
  \item{winner}{numeric or character representation of the mass of the winner to plot.}
  \item{legend}{a character or expression vector of length >= 1 to appear in the legend.  If \code{NULL}, the legend is not displayed.}
  \item{bin}{two element numeric vector of the range for the winner bin.  If \code{NULL}, the bin highlight rectangle is not displayed.}
  \item{xlim}{range of x axis.  If \code{NULL}, \code{pad} and/or \code{bin} are used.}
  \item{pad}{numeric multiplier to calculate \code{xlim}.  If \code{bin} is \code{NULL}, \code{pad} defaults to \code{5} and \code{xlim} defaults to \code{winner + pad * c(-5, 5)}.  If \code{bin} is not \code{NULL}, \code{xlim} defaults to \code{bin + pad * c(-2, 2)}.}
  \item{lty,lwd}{vector of line types and widths.  The first element is for the first plot, the second element for the second plot, etc., even if lines are not plotted for all elements.  Line types will be used cyclically until all plots are drawn.}
  \item{col}{vector of colors.  Colors are used cyclically.}
  \item{add}{logical.  If \code{TRUE}, plots are added to current one.  \code{legend} and \code{bin} are ignored.}
  \item{\dots}{Graphical parameters (see \code{\link{par}}) and any further arguments of \code{\link{matplot}} may also be supplied as arguments to this function.}
}
\author{
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\seealso{
  \code{\link{winnerplots}}
}
\keyword{hplot}
