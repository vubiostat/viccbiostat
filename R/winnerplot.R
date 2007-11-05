winnerplot <- function(data, winner, legend=NULL, bin=NULL, xlim=NULL, pad=diff(bin), lty=par("lty"), lwd=par("lwd"), col=par("col"), add=FALSE, ...)
{
    if (missing(xlim))
    {
        if (is.null(pad))
            pad <- 5
        if (is.null(bin))
            xlim <- as.numeric(winner) + pad * c(-5, 5)
        else
            xlim <- bin + pad * c(-2, 2)
    }
    x <- data[,1]
    xi <- which(x < xlim[2] & x > xlim[1])
    x <- x[xi]
    y <- data[xi, -1]
    ylim <- c(min(y), max(y))
    matplot(x, y, type="l", lty=lty, lwd=lwd, col=col, xlab="Mass", ylab="Intensity", main=paste("Winner",winner), add=add, ...)
    if (!missing(bin) && !add)
        rect(bin[1], ylim[1], bin[2], ylim[2], col=NA, border="yellow")
    if (missing(legend) && !add)
        legend <- colnames(data)[-1]
    if (!is.null(legend) && !add)
        legend("topright", legend, lty=lty, lwd=lwd, col=col)
}

winnerplots <- function(data, winners, legend=NULL, bins=NULL, lty=1, lwd=1, col=1:6, ...)
{
    for (i in 1:length(winners))
    {
        winnerplot(data=data, winner=winners[i], legend=legend, bin=bins[i,], lty=lty, lwd=lwd, col=col, add=FALSE, ...)
    }
}
