multidensityplot <- function(data, type="l", xlim=NULL, ylim=NULL, log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL, legend=NULL, ann=par("ann"), axes=TRUE, frame.plot=axes, panel.first=NULL, panel.last=NULL, asp=NA, col=par("col"), lwd=par("lwd"), lty=par("lty"), ...)
{
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)

    dens <- lapply(data, density)
    x <- c(sapply(dens, function(d) d$x))
    y <- c(sapply(dens, function(d) d$y))
    xlim <- if (is.null(xlim)) range(x[is.finite(x)]) else xlim
    ylim <- if (is.null(ylim)) range(y[is.finite(y)]) else ylim

    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    mapply(function(d, type, col, lwd, lty) lines(d$x, d$y, type=type, col=col, lwd=lwd, lty=lty, xlim=xlim, ylim=ylim, ...), dens, type, col, lwd, lty)
    if (axes)
    {
        localAxis(x, side=1, ...)
        localAxis(y, side=2, ...)
    }
    if (frame.plot) localBox(...)
    if (ann)
    {
        localTitle(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
        if (missing(legend)) legend <- names(data)
        if (!is.null(legend)) legend("topright", legend=legend, col=col, lty=lty, lwd=lwd)
    }
    invisible()
}
