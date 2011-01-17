dsp <- function(x, ...) UseMethod("dsp")

dsp.default <- function(x, y, p.pch=19, p.col=1, p.cex=.8, bkgr=T, ...) {
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
    dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2 # local offset
    dat$lx <- dat$ly + ((ceiling(sqrt(hm - 1)) ** 2 == hm - 1) & (hm > 1)) / (box.size + 1) / 2
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

    points(dat$x + coord[sc[dat$idx, 1]] + dat$lx, dat$y + coord[sc[dat$idx, 2]] + dat$ly, pch=dat$pch, col=dat$col, cex=p.cex)

    table(factor(y, levels=rev(min(y):max(y))), factor(x, levels=min(x):max(x)))
}

dsp.formula <- function(formula, data=parent.frame(), ..., subset) {
    if (missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")

    enquote <- function(x) { as.call(list(as.name("quote"), x)) }

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)

    args <- lapply(m$..., eval, data, parent.frame())
    nmargs <- names(args)
    if ("main" %in% nmargs) args[["main"]] <- enquote(args[["main"]])
    if ("sub" %in% nmargs) args[["sub"]] <- enquote(args[["sub"]])
    if ("xlab" %in% nmargs) args[["xlab"]] <- enquote(args[["xlab"]])
    if ("ylab" %in% nmargs) args[["ylab"]] <- enquote(args[["ylab"]])

    m$... <- NULL
    #m$na.action <- na.pass
    subset.expr <- m$subset
    m$subset <- NULL
    require(stats, quietly=TRUE) || stop("package 'stats' is missing")
    m[[1]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")

    if (!missing(subset)) {
        s <- eval(subset.expr, data, parent.frame())
        n <- nrow(mf)
        dosub <- function(x) { if (length(x) == n) x[s] else x }
        args <- lapply(args, dosub)
        mf <- mf[s,]
    }
    do.call("dsp", c(list(mf[[response]], mf[[-response]]), args))
}
