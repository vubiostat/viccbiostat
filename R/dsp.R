## Scatter plot for discrete data
## For something like baseball scores
## Only works with integers...

dsp <- function(x, y, p.pch=19, p.col=1, p.cex=.8, bkgr=T, ...) {
    # Scatter plot for discrete data
    # Only works with 'integer-valued' data.
    if (any(x!=round(x), na.rm=T) | any(y!=round(y), na.rm=T)) { stop('This only works with integers.  Sorry.', '\n') }

    L <- length(x)
    cc <- complete.cases(x, y)

    if (length(p.pch) < L) { p.pch <- rep(p.pch, length.out=L) }
    if (length(p.col) < L) { p.col <- rep(p.col, length.out=L) }
    if (length(p.cex) < L) { p.cex <- rep(p.cex, length.out=L) }

    x <- x[cc]
    y <- y[cc]
    p.pch <- p.pch[cc]
    p.col <- p.col[cc]
    p.cex <- p.cex[cc]

    x.levels <- sort(unique(x))
    y.levels <- sort(unique(y))
    tab <- table(x, y)
    max.freq <- max(tab)
    box.size <- ceiling(sqrt(max.freq))
    X <- range(x) + c(0, 1)
    Y <- range(y) + c(0, 1)
    plot(X, Y, las=1, type='n', xaxs='i', yaxs='i', bty='n', xaxt='n', yaxt='n', ...)
    axis(1, at=pretty(x)+.5, labels=pretty(x), tick=F, las=1)
    axis(2, at=pretty(y)+.5, labels=pretty(y), tick=F, las=1)

    if (!bkgr) {
        for (i in y.levels) { segments(min(x), i, max(x), i, col=grey(.9)) }
        for (i in x.levels) { segments(i, min(y), i, max(x), col=grey(.9)) }
    }

    every.other.element.x <- function(n) {
        # to make 1,n,2,n,3,n, ..., n,n vector
        c(rbind(1:n, rep(n, n)))[-(2*n)]
    }
    every.other.element.y <- function(n) {
        c(rbind(rep(n, n), 1:n))[-(2*n)]
    }

    square.coordinates <- function(box.size) {
        x.c <- y.c <- 1
        for (i in 2:box.size) { x.c <- c(x.c, every.other.element.x(i)) }
        for (j in 2:box.size) { y.c <- c(y.c, every.other.element.y(j)) }
        data.frame(x.c, y.c)
    }

    sc <- square.coordinates(box.size)
    coord <- (1:box.size) / (box.size+1)
    off.set <- coord[1]/4

    grey.scale <- rev(seq(.65, .95, length=max.freq))

    dat <- data.frame(id=1:length(x), x, y)
    dat <- dat[order(dat$x, dat$y),]
    within <- c(t(tab))
    within <- within[within > 0]
    idx <- NULL
    hm <- NULL
    for (i in within) {
        idx <- c(idx, 1:i) # index within category
        hm <- c(hm, rep(i, i))
    }
    dat$idx <- idx
    dat$lo <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2 # local offset
    dat <- dat[order(dat$id),]
    dat$col <- p.col
    dat$pch <- p.pch

    if (bkgr) {
        for (i in x.levels) {
            for (j in y.levels) {
                n <- sum(x==i & y==j)
                if (n > 0) { rect(i+off.set, j+off.set, i+1-off.set, j+1-off.set, border=grey(grey.scale[n]), col=grey(grey.scale[n])) }
            }
        }
    }

    points(dat$x + coord[sc[dat$idx, 1]] + dat$lo, dat$y + coord[sc[dat$idx, 2]] + dat$lo, pch=dat$pch, col=dat$col, cex=p.cex)

    table(factor(y, levels=rev(min(y):max(y))), factor(x, levels=min(x):max(x)))
}
