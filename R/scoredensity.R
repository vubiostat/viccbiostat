scoredensity <- function(x, y, z, main, legend, boxplots=TRUE, bw=5, xcol="red", ycol="blue", zcol="green", xlty=1, ylty=2, zlty=1)
{
    denx <- density(x, bw=bw)
    deny <- density(y, bw=bw)

    ylim <- c(0, max(denx$y, deny$y))
    xlim <- c(min(denx$x, deny$x, z), max(denx$x, deny$x, z))

    if (boxplots)
    {
        par(mfrow=c(2,1))

        boxplot(x, y, horizontal=TRUE, ylim=xlim, col=c("gray90","gray50"), main=main)
    }

    plot(x=denx$x, y=denx$y, type="l", col=xcol, lty=xlty, xlim=xlim, ylim=ylim, ylab="Probability Density", xlab="Profile Score")
    lines(x=deny$x, y=deny$y, col=ycol, lty=ylty)
    abline(v=z, lwd=3, col=zcol, lty=zlty)

    legend("topright", legend=legend, lty=c(xlty, ylty, zlty), col=c(xcol, ycol, zcol), cex=0.57, adj=c(0,0.5))
}
