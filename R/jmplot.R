jmplot <- function(x, y, levels, names=NULL, xlim=NULL, ylim=NULL, log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL, xratio=.8, yratio=xratio, show.n=FALSE, ann=par("ann"), axes=TRUE, frame.plot=axes, panel.first=NULL, panel.last=NULL, asp=NA, ...) {

    localTplot <- function(..., type="b", horizontal=FALSE) tplot(..., type=type, axes=FALSE, horizontal=horizontal)
    eliminateTplot <- function(func, ..., type, dist, jit, names, group.col, boxcol, boxborder, group.pch, median.line, mean.line, median.pars, mean.pars, boxplot.pars, my.gray, axes, frame.plot, add, horizontal) func(...)

    localPlot <- function(xy, ..., lwd) eliminateTplot(plot.xy, xy, "p", ...)
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(axis, ...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(box, ...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(plot.window, ...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) eliminateTplot(title, ...)

    levels <- as.factor(levels)
    xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)), log)

    # Set up defaults
    if (is.null(names)) names <- levels(levels)
    if (is.null(xlab)) xlab <- xy$xlab
    if (is.null(ylab)) ylab <- xy$ylab

    # Save plotting parameters
    pars <- par(no.readonly=TRUE)
    mar <- pars$mar
    # Set the layout
    layout(matrix(c(1,3,0,2), 2), widths=c(xratio,1-xratio), heights=c(1-yratio,yratio))
    par(mar=c(0,0,0,0), oma=c(0,0,mar[3],mar[4])+pars$oma)

    # Calculate xlim, ylim
    lim <- function(z) {
        r <- range(z, na.rm=TRUE, finite=TRUE)
        #pm <- diff(r) / 20
        #r + pm * c(-1,1)
    }
    if (is.null(xlim)) xlim <- lim(xy$x)
    if (is.null(ylim)) ylim <- lim(xy$y)

    # plot X distribution on top
    par(mar=c(0,mar[2],0,0))
    localTplot(x~levels, ylim=xlim, horizontal=TRUE, show.n=FALSE, ...)
    if (axes) localAxis(side=2, at=1:nlevels(levels), labels=names, ...)

    # plot Y distribution on right
    par(mar=c(mar[1],0,0,0))
    localTplot(y~levels, ylim=ylim, show.n=show.n, ...)
    if (axes) localAxis(side=1, at=1:nlevels(levels), labels=names, ...)

    # plot X-Y points
    par(mar=c(mar[1],mar[2],0,0))
    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    panel.first
    localPlot(xy, xlim=xlim, ylim=ylim, ...)
    panel.last

    # add axes
    if (axes) {
        localAxis(side=1, ...)
        localAxis(side=2, ...)
    }
    if (frame.plot) localBox(...)
    # add titles
    if (ann) {
        localTitle(sub=sub, xlab=xlab, ylab=ylab, ...)
        localTitle(main=main, outer=TRUE, ...)
    }

    # Restore plotting parameters
    par(pars)
}

#x <- rexp(100)
#y <- rexp(100)
#levels <- as.factor(sample(c("Male","Female"), 100, TRUE))
#jmplot(x, y, levels, col=levels)
