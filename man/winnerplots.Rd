\name{winnerplots}
\alias{winnerplots}
\title{Winner Plots}
\description{
  Plot the average spectra for a set of winners.  A multi-variate version of \code{\link{winnerplot}}.
}
\usage{
winnerplots(data, winners, legend = NULL, bins = NULL, lty = 1,
            lwd = 1, col = 1:6, \dots)
}
\arguments{
  \item{data}{numeric matrix of average spectra data.  The first column should be the x coordinates (mass) and each subsequent column should be y coordinates.}
  \item{winners}{numeric vector of the mass of the winners to plot.}
  \item{legend}{a character or expression vector of length >= 1 to appear in the legend.  If \code{NULL}, the legend is not displayed.}
  \item{bins}{two column numeric matrix of the ranges for the winner bin, one row for each winner.  If \code{NULL}, the bin highlight rectangle is not displayed.}
  \item{lty,lwd}{vector of line types and widths.  The first element is for the first plot, the second element for the second plot, etc., even if lines are not plotted for all elements.  Line types will be used cyclically until all plots are drawn.}
  \item{col}{vector of colors.  Colors are used cyclically.}
  \item{\dots}{Graphical parameters (see \code{\link{par}}) and any further arguments of \code{\link{winnerplot}} or \code{\link{matplot}} may also be supplied as arguments to this function.}
}
\author{
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\seealso{
  \code{\link{winnerplot}}
}
\keyword{hplot}
