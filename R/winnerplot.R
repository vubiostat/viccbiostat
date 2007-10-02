winnerplot <- function(data, winner, bin=NULL, xlim=NULL, legend=NULL, pad=diff(bin), lty=par("lty"), lwd=par("lwd"), col=par("col"), ...)
{
    if (missing(xlim))
    {
        if (is.null(pad))
            pad <- 5
        if (missing(bin))
            xlim <- winner + c(-5 * pad, 5 * pad)
        else
            xlim <- bin + c(-2 * pad, 2 * pad)
    }
    x <- data[,1]
    xi <- which(x < xlim[2] & x > xlim[1])
    x <- x[xi]
    y <- data[xi, -1]
    ylim <- c(min(y), max(y))
    matplot(x, y, type="l", lty=lty, lwd=lwd, col=col, xlab="Mass", ylab="Intensity", main=paste("Winner",winner), ...)
    if (!missing(bin))
        rect(bin[1], ylim[1], bin[2], ylim[2], col=NA, border="yellow")
    if (missing(legend))
        legend <- colnames(data)[-1]
    if (!is.null(legend))
        legend("topright", legend, cex=.6, lty=lty, lwd=lwd, col=col, ...)
}

winnerplots <- function(data, winners, bins, legend, lty=1, lwd=1, col=1:6, ...)
{
    for (i in 1:length(winners))
    {
        winnerplot(data=data, winner=winners[i], bin=bins[i,], legend=legend, lty=lty, lwd=lwd, col=col, ...)
    }
}
