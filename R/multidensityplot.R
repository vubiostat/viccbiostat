multidensityplot <- function(data, legend=NULL, type="l", lty=par("lty"), lwd=1, pch=NULL, col=1:6, ...)
{
    if (class(data[[1]]) == "density")
        dens <- data
    else
        dens <- lapply(data, density, na.rm=TRUE)
    x <- sapply(dens, function(d) d$x)
    y <- sapply(dens, function(d) d$y)
    matplot(x, y, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ...)
    if (missing(legend)) legend <- names(data)
    if (!is.null(legend)) legend("topright", legend=legend, lty=lty, lwd=lwd, pch=pch, col=col)
    invisible(dens)
}
