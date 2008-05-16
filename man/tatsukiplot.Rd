\name{tatsukiplot}
\alias{tatsukiplot}
\alias{tatsukiplot.formula}
\alias{tatsukiplot.default}
\title{Dot Plots}
\description{
  Produce dot plot(s) of the given (grouped) values.
}
\usage{
tatsukiplot(x, \dots)

\method{tatsukiplot}{formula}(formula, data = NULL, \dots,
        subset, na.action = NULL)

\method{tatsukiplot}{default}(x, \dots, type = "d", dist = NULL, jit = 0.05,
        names, ylim = NULL, main = NULL, sub = NULL,
        xlab = NULL, ylab = NULL, col = par("col"), pch = par("pch"),
        group.col = FALSE, group.pch = FALSE, median.line = FALSE,
        mean.line = FALSE, median.pars = list(col = par("col")),
        mean.pars = median.pars, boxplot.pars = NULL, show.n = FALSE,
        my.gray = gray(0.75), ann = par("ann"), axes = TRUE,
        frame.plot = axes, add = FALSE, at = NULL, horizontal = FALSE)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ grp}, where \code{y} is a numeric vector of data values to be split into groups according to the grouping variable \code{grp} (usually a factor).}
  \item{data}{a data.frame (or list) from which the variables in \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations to be used for plotting.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}s.  The default is to ignore missing values in either the response or the group.}
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
  \item{ylim}{the y limits of the plot.}
  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis, defaults to nothing.}
  \item{ylab}{a label for the y axis, defaults to nothing.}
  \item{col}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.}
  \item{pch}{character or integer code for kind of points, see \code{\link{points.default}}.}
  \item{group.col}{a logical value indicating whether values in \code{col} correspond to individual values or groups of values.}
  \item{group.pch}{a logical value indicating whether values in \code{pch} correspond to individual values or groups of values.}
  \item{median.line, mean.line}{a logical value indicating whether the medians and or means should be plotted.}
  \item{median.pars, mean.pars}{additional graphical parameters to pass to \code{\link{lines}}.}
  \item{boxplot.pars}{additional graphical parameters to pass to \code{\link{boxplot}}.}
  \item{show.n}{a logical value indicating whether there the sample sizes should be printed along axis 3.}
  \item{my.gray}{color code or name, see \code{\link{colors}}, \code{link{pallette}}.  The color of the secondary (background) plots.}
  \item{ann}{a logical value indicating whether the default annotation (title and x and y axis labels) should appear on the plot.}
  \item{axes}{a logical value indicating whether both axes should be drawn on the plot.  Use graphical parameter \code{"xaxt"} or \code{"yaxt"} to suppress just one of the axes.}
  \item{frame.plot}{a logical value indicating whether a box should be drawn around the plot.}
  \item{add}{logical, if true \emph{add} tatsukiplot to current plot.}
  \item{at}{numeric vector giving the locations where the tatsukiplots should be drawn, particularly when \code{add = TRUE}; defaults to \code{1:n} where \code{n} is the number of groups.}
  \item{horizontal}{logical indicating if the plots should be horizontal; default FALSE means vertical.}
}
\details{
  The arguments \code{type}, \code{col}, \code{pch} may be vectors and will be recycled.
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
set.seed(5)
v <- rnorm(50, seq(66, 70, length=50), .2)
g <- paste('Group', sample(LETTERS[1:5], 50, prob=c(.1,.2,.3,.3,.1), TRUE))
sex <- sample(1:2, 50, TRUE)
}
tatsukiplot(v ~ g, dist=0.1, jit=0.1, ylab='Values', col=sex,
        pch=c(1,19,3,8,15), group.pch=TRUE, cex=0.75)
}
\keyword{hplot}
