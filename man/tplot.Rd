\name{tplot}
\alias{tplot}
\alias{tplot.formula}
\alias{tplot.default}
\title{Dot Plots}
\description{
  Produce dot plot(s) of the given (grouped) values.
}
\usage{
tplot(x, \dots)

\method{tplot}{formula}(formula, data = parent.frame(), \dots, subset)

\method{tplot}{default}(x, \dots, type = "d", dist = NULL, jit = 0.05, names,
            xlim = NULL, ylim = NULL, main = NULL, sub = NULL, xlab = NULL,
            ylab = NULL, col = par("col"), pch = par("pch"), col = par("col"),
            group.col = FALSE, group.pch = FALSE, group.col = FALSE,
            median.line = FALSE, mean.line = FALSE, median.pars = list(col = par("col")),
            mean.pars = median.pars, boxplot.pars = NULL, show.n = FALSE,
            ann = par("ann"), axes = TRUE, frame.plot = axes, add = FALSE,
            at = NULL, horizontal = FALSE, panel.first = NULL, panel.last = NULL)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ grp}, where \code{y} is a numeric vector of data values to be split into groups according to the grouping variable \code{grp} (usually a factor).}
  \item{data}{a data.frame (or list) from which the variables in \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations to be used for plotting.}
  \item{x}{for specifying data from which the boxplots are to be produced.  Either a numeric vector, or a single list containing such vectors.  Additional unnamed arguments specify further data as separate vectors (each corresponding to a component dotplot).  \code{\link{NA}}s are allowed in the data.}
  \item{\dots}{For the \code{formula} method, named arguments to be passed to the default method.

  For the default method, unnamed arguments are additional data vectors (unless \code{x} is a list when they are ignored), and named arguments are extra graphical parameters (see \code{\link{par}}).}
  \item{type}{character vector giving the type of plot desired for each group.  The following values are possible:
    \code{"d"} for dots,
    \code{"b"} for boxplots,
    \code{"db"} for dots over boxplots,
    \code{"bd"} for boxplots over dots.}
  \item{dist}{numeric value for a distance threshold for points to be jittered.  Defaults to \code{diff(range(x))/100}.}
  \item{jit}{numeric value of how much to jitter.}
  \item{names}{group labels which will be printed under each boxplot.}
  \item{xlim}{the x limits of the plot.}
  \item{ylim}{the y limits of the plot.}
  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis, defaults to nothing.}
  \item{ylab}{a label for the y axis, defaults to nothing.}
  \item{col}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.  Specifies the foreground plot color.}
  \item{pch}{character or integer code for kind of points, see \code{\link{points.default}}.}
  \item{backcol}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.  Specifies the background plot color.}
  \item{group.col}{a logical value indicating whether values in \code{col} correspond to individual values or groups of values.}
  \item{group.pch}{a logical value indicating whether values in \code{pch} correspond to individual values or groups of values.}
  \item{group.backcol}{a logical value indicating whether values in \code{backcol} correspond to individual values or groups of values.}
  \item{median.line, mean.line}{a logical value indicating whether the medians and or means should be plotted.}
  \item{median.pars, mean.pars}{additional graphical parameters to pass to \code{\link{lines}}.}
  \item{boxplot.pars}{additional graphical parameters to pass to \code{\link{boxplot}}.}
  \item{show.n}{a logical value indicating whether there the sample sizes should be printed along axis 3.}
  \item{ann}{a logical value indicating whether the default annotation (title and x and y axis labels) should appear on the plot.}
  \item{axes}{a logical value indicating whether both axes should be drawn on the plot.  Use graphical parameter \code{"xaxt"} or \code{"yaxt"} to suppress just one of the axes.}
  \item{frame.plot}{a logical value indicating whether a box should be drawn around the plot.}
  \item{add}{logical, if true \emph{add} tplot to current plot.}
  \item{at}{numeric vector giving the locations where the tplots should be drawn, particularly when \code{add = TRUE}; defaults to \code{1:n} where \code{n} is the number of groups.}
  \item{horizontal}{logical indicating if the plots should be horizontal; default FALSE means vertical.}
  \item{panel.first}{an expression to be evaluated after the plot axes are set up but before any plotting takes place. This can be useful for drawing background grids or scatterplot smooths.}
  \item{panel.last}{an expression to be evaluated after plotting has taken place.}
}
\details{
  The arguments \code{type}, \code{col}, \code{pch} may be vectors and will be recycled.
}
\value{
  A list of data.frames of the plotting measurement, color, and character used for each datum, split by group.
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
\seealso{
  \code{\link{boxplot}}, \code{\link{dotplot}}.
}
\examples{
\dontshow{
set.seed(100)
age <- rnorm(80, rep(c(26,36), c(70,10)), 4)
sex <- sample(c('Female','Male'), 80, TRUE)
group <- paste('Group', sample(1:4, 40, prob=c(2,5,4,1), replace=TRUE))
}
tplot(age ~ group, show.n=TRUE, type=c('db','db','db','d'), col=sex, dist=0.2, jit=0.05, las=1, pch=19)
\dontshow{
legend('topright', pch=19, col=1:2, legend=c('Female','Male'))
}
}
\keyword{hplot}
