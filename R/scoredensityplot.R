scoredensityplot <- function(x, y, z=NULL, main, legend, boxplots=TRUE, bw, xcol="red", ycol="blue", zcol="green", xlty=1, ylty=2, zlty=1)
{
    result <- list()

    result$density.x <- density(x)
    result$density.y <- density(y)

    ylim <- c(0, max(result$density.x$y, result$density.y$y))
    xlim <- c(min(result$density.x$x, result$density.y$x, z), max(result$density.x$x, result$density.y$x, z))

    if (boxplots)
    {
        par(mfrow=c(2,1))

        result$boxplot <- boxplot(x, y, horizontal=TRUE, ylim=xlim, col=c("gray90","gray50"), main=main, names=legend[1:2])
    }

    plot(x=result$density.x$x, y=result$density.x$y, type="l", col=xcol, lty=xlty, xlim=xlim, ylim=ylim, ylab="Probability Density", xlab="Profile Score")
    lines(x=result$density.y$x, y=result$density.y$y, col=ycol, lty=ylty)

    if (!missing(z)) abline(v=z, lwd=3, col=zcol, lty=zlty)

    legend("topright", legend=legend, lty=c(xlty, ylty, zlty), col=c(xcol, ycol, zcol), cex=0.7, adj=c(0,0.5))

    invisible(result)
}
