scoredensityplot <- function(x, y, z=NULL, main=NULL, legend=NULL, boxplots=TRUE, split.screen=TRUE, type="l", lty=c(1,2,1), lwd=c(1,1,3), pch=NULL, col=c("red","blue","green"), xlim=NULL, ylim=NULL, ...) {
    result <- list()
    result$density.x <- density(x)
    result$density.y <- density(y)

    xm <- cbind(result$density.x$x, result$density.y$x)
    ym <- cbind(result$density.x$y, result$density.y$y)
    xlim <- if (missing(xlim)) range(xm, na.rm=TRUE, finite=TRUE) else xlim
    ylim <- if (missing(ylim)) range(ym, na.rm=TRUE, finite=TRUE) else ylim

    if (boxplots) {
        if (split.screen) {
            mfrow <- par("mfrow")
            par(mfrow=c(2,1))
        }
        result$boxplot <- boxplot(x, y, horizontal=TRUE, ylim=xlim, col=c("gray90","gray50"), main=main, names=legend[1:2])
    }

    lz <- length(z)

    if (length(lty) < 3)
        lty <- rep(lty, length.out=3)
    lty <- c(lty[1:2], rep(lty[-c(1:2)], length.out=lz))
    if (length(lwd) < 3)
        lwd <- rep(lwd, length.out=3)
    lwd <- c(lwd[1:2], rep(lwd[-c(1:2)], length.out=lz))
    if (length(pch) < 3)
        pch <- rep(pch, length.out=3)
    pch <- c(pch[1:2], rep(pch[-c(1:2)], length.out=lz))
    if (length(col) < 3)
        col <- rep(col, length.out=3)
    col <- c(col[1:2], rep(col[-c(1:2)], length.out=lz))

    matplot(xm, ym, type=type, lty=lty, lwd=lwd, pch=pch, col=col, ylab="Probability Density", xlab="Profile Score", xlim=xlim, ylim=ylim, ...)

    if (!missing(z))
        abline(v=z, lty=lty[-c(1:2)], lwd=lwd[-c(1:2)], col=col[-c(1:2)])

    if (!missing(legend))
        legend("topright", legend=legend, lty=lty, lwd=lwd, pch=pch, col=col)

    if (boxplots && split.screen)
        par(mfrow=mfrow)
    invisible(result)
}
