\name{multidensityplot}
\alias{multidensityplot}
\title{Multi-Density Plot}
\description{
  Plot multiple densities on the same device.
}
\usage{
multidensityplot(data, legend = NULL, type = "l", lty = par("lty"),
                 lwd = 1, pch = NULL, col = 1:6, \dots)
}
\arguments{
  \item{data}{list of \code{\link{density}} objects or anything that has an S3 density function.  If not \code{density} objects, the density is computed using the default arguments.}
  \item{legend}{a character or expression vector of length >= 1 to appear in the legend.  If \code{NULL}, the legend is not displayed.}
   \item{type}{character string (length 1 vector)  or vector of 1-character strings indicating the type of plot for each element of \code{data}, see \code{\link{plot}} for all possible \code{\link{type}}s.  The first character of \code{\link{type}} defines the first plot, the second character the second, etc.  Characters in \code{\link{type}} are cycled through; e.g., \code{"pl"} alternately plots points and lines.}
   \item{lty,lwd}{vector of line types and widths.  The first element is for the first plot, the second element for the second plot, etc., even if lines are not plotted for all elements.  Line types will be used cyclically until all plots are drawn.}
   \item{pch}{character string or vector of 1-characters or integers for plotting characters, see \code{\link{points}}.  The first character is the plotting-character for the first plot, the second for second, etc.  The default is the digits (1 through 9, 0) then the letters.}
   \item{col}{vector of colors.  Colors are used cyclically.}
   \item{\dots}{Graphical parameters (see \code{\link{par}}) and any further arguments of \code{\link{matplot}} may also be supplied as arguments to this function.}
}
\details{
  It is highly recommended to use \code{type="l"}.  Use different values of \code{lty} for printing in black & white.
}
\value{
  list of the densities of the elements in \code{data} or \code{data} itself.
}
\author{
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\seealso{
  \code{\link{scoredensityplot}}
}
\keyword{hplot}
