\name{scoredensityplot}
\alias{scoredensityplot}
\title{Score Density Plot}
\description{
Plot the boxplot and density of the scores for two groups and any additional samples.
}
\usage{
scoredensityplot(x, y, z = NULL, main = NULL, legend = NULL, boxplots = TRUE, split.screen=TRUE, type = "l", lty = c(1,2,1), lwd = c(1,1,3), pch = NULL, col = c("red", "blue", "green"), xlim = NULL, ylim = NULL, \dots)
}
\arguments{
    \item{x,y}{numeric vectors of score data for density and box plotting.}
    \item{z}{numeric, vector of scores to plot as vertical lines.}
    \item{main}{a main title for the plot, see also \code{\link{title}}.}
    \item{legend}{a character or expression vector of length >= 1 to appear in the legend.  If \code{NULL}, the legend is not displayed.}
    \item{boxplots}{logical. If \code{TRUE}, include the boxplots of \code{x} and \code{y} above the density plot.}
    \item{split.screen}{logical.  If \code{TRUE}, put the density and box plots on a single device with two rows.}
    \item{type}{character string (length 1 vector)  or vector of 1-character strings indicating the type of plot for \code{x}, \code{y}, and each element of \code{z}, see \code{\link{plot}} for all possible \code{\link{type}}s.  The first character of \code{\link{type}} defines \code{x}, the second character \code{y}.  Characters in \code{\link{type}} are cycled through.}
    \item{lty,lwd}{vector of line types and widths.  The first element is for \code{x}, the second element for \code{y}, and remaining elements are cycled through for \code{z}, even if lines are not plotted for elements.}
    \item{pch}{character string or vector of 1-characters or integers for plotting characters, see \code{\link{points}}.  The first character is the plotting-character for \code{x}, the second for \code{y}, and the remaining are cycled through for \code{z}.  The default is the digits (1 through 9, 0) then the letters.}
    \item{col}{vector of colors.  Colors are used cyclically except for the first two.}
    \item{xlim,ylim}{ranges of x and y axes, as in \code{\link{plot}}.}
    \item{\dots}{Graphical parameters (see \code{\link{par}}) and any further arguments of \code{\link{matplot}} may also be supplied as arguments to this function.}
}
\details{
The first elements of \code{col, lty, lwd, pch, type} are used for \code{x} and the second elements are used for \code{y}.  The remaining elements are cycled for elements of \code{z}.

It is highly recommended to use \code{type="l"}.  Use different values of \code{lty} for printing in black & white.
}
\value{
    \item{density.x,density.y}{density of \code{x} and \code{y}, respectively.}
}
\author{
Will Gray\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{w.gray@vanderbilt.edu}
}
\seealso{
\code{\link{multidensityplot}}
}
\keyword{hplot}
