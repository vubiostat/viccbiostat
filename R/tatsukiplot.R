tatsukiplot <- function(x, ...)  UseMethod("tatsukiplot")

tatsukiplot.default <- function(x, ..., type="d", dist=NULL, jit=0.05, names, ylim=NULL, main=NULL, sub=NULL, xlab=NULL, ylab=NULL, col=par("col"), pch=par("pch"), group.col=FALSE, group.pch=FALSE, median.line=FALSE, mean.line=FALSE, median.pars=list(col=par("col")), mean.pars=median.pars, boxplot.pars=NULL, show.n=FALSE, my.gray=gray(.75), ann=par("ann"), axes=TRUE, frame.plot=axes, add=FALSE, at=NULL, horizontal=FALSE) {
    localAxis <- function(..., bg, cex, lty, lwd) axis(...)
    localBox <- function(..., bg, cex, lty, lwd) box(...)
    localWindow <- function(..., bg, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., bg, cex, lty, lwd) title(...)

    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names))
        attributes(args)$names != ""
    else logical(length(args))
    groups <- if (is.list(x))
        x
    else args[!namedargs]
    if ((n <- length(groups)) == 0)
        stop("invalid first argument")
    if (length(class(groups)))
        groups <- unclass(groups)
    if (!missing(names))
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names")))
            attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }

    ng <- length(groups) # number of groups
    l <- sapply(groups, length) # size of each group
    g <- rep(1:ng, l) # groups as.numeric
    nv <- sum(l) # total count

    # set y scale
    ylim <- if (!is.null(ylim))
        ylim
    else {
        r <- range(groups, na.rm=TRUE)
        pm <- diff(r) / 20
        r + pm * c(-1,1)
    }
    # set x scale
    xlim <- c(0.5, ng+0.5)

    at <- if (is.null(at)) 1:ng else at
    if (length(at) != ng)
        stop("'at' must have same length as the number of groups")

    xlab <- if (is.null(xlab)) "" else xlab
    ylab <- if (is.null(ylab)) "" else ylab
    main <- if (is.null(main)) "" else main
    sub <- if (is.null(sub)) "" else sub

    type <- match.arg(type, choices=c("d","db","bd","b"), several.ok=TRUE)
    # type of plot for each group
    if ((length(type) > 1) && (length(type) != ng))
        warning("length of 'type' does not match the number of groups")
    type <- rep(type, length.out=ng)

    # Use colors by group
    if (group.col) {
        if (length(col) != ng)
            warning("length of 'col' does not match the number of groups")
        g.col <- rep(col, length.out=ng)
        col <- rep(g.col, l)
    # Use colors by individual or global
    } else {
        if((length(col) > 1) && (length(col) != nv))
            warning("length of 'col' does not match the number of data points")
        col <- rep(col, length.out=nv)
        g.col <- rep(1, length.out=ng)
    }

    # Use plot characters by group
    if (group.pch) {
        if (length(pch) != ng)
            warning("length of 'pch' does not match the number of groups")
        pch <- rep(rep(pch, length.out=ng), l)
    # Use plot characters by individual or global
    } else {
        if((length(pch) > 1) && (length(pch) != nv))
            warning("length of 'pch' does not match the number of data points")
        pch <- rep(pch, length.out=nv)
    }

    # split colors and plot characters into groups
    col <- split(col, g)
    pch <- split(pch, g)
    # remove any NAs from the data and options
    nonas <- lapply(groups, function(x) !is.na(x))
    groups <- mapply("[", groups, nonas, SIMPLIFY=FALSE)
    col <- mapply("[", col, nonas, SIMPLIFY=FALSE)
    pch <- mapply("[", pch, nonas, SIMPLIFY=FALSE)

    # whether or not to display a mean and median line for each group
    mean.line <- rep(mean.line, length.out=ng) 
    median.line <- rep(median.line, length.out=ng)

    # set defaults for dist and jit
    if (is.null(dist) || is.na(dist)) dist <- diff(range(ylim)) / 100
    if (is.null(jit) || is.na(jit)) jit <- 0.025 * ng

    # 1 2 3 1 3 2 1 1 4 2
    # -------------------
    # 1 1 1 2 2 2 3 4 1 3
    how.many.so.far <- function(g) {
        out <- NULL
        u <- unique(g)
        for (i in 1:length(u)) out[which(g==u[i])] <- 1:sum(g==u[i])
        out
    }
    # turns the values in each group into their plotting points
    grouping <- function(v, dif) {
        vs <- sort(v)
        together <- c(FALSE, diff(vs) <= dif)
        g.id <- cumsum(!together)
        g.si <- rep(x<-as.vector(table(g.id)), x)
        vg <- cbind(vs, g.id, g.si)[rank(v),]
        if (length(v)==1) vg <- as.data.frame(t(vg))
        hmsf <- how.many.so.far(vg[,2])
        data.frame(vg, hmsf)
    }
    groups <- lapply(groups, grouping, dif=dist)

    # set up new plot
    if (!add) {
        plot.new()
        if (horizontal)
            do.call("localWindow", c(list(ylim, xlim), args[namedargs]))
        else
            do.call("localWindow", c(list(xlim, ylim), args[namedargs]))
    }

    # function to compute the jittering
    jit.f2 <- function(g.si, hm.sf) { hm.sf - (g.si + 1) / 2 }

    out <- list()

    Lme <- 0.2 * c(-1, 1)
    for (i in 1:ng) {
        to.plot <- groups[[i]]
        gs <- to.plot$g.si 
        hms <- to.plot$hm
        x <- rep(at[i], nrow(to.plot)) + jit.f2(gs, hms) * jit
        y <- to.plot$vs

        if (type[i] == "bd") { # dots behind
            if (horizontal)
                do.call("points", c(list(x=y, y=x, pch=pch[[i]], col=my.gray), args[namedargs]))
            else
                do.call("points", c(list(x=x, y=y, pch=pch[[i]], col=my.gray), args[namedargs]))
        }
        if (type[i] %in% c("bd", "b")) { # boxplot in front
            outliers <- do.call("boxplot", c(list(x=y, at=at[i], add=TRUE, axes=FALSE, border=g.col[i], outline=FALSE, horizontal=horizontal), boxplot.pars))$out
            if (type[i] == "b") {
                toplot <- rowSums(outer(y, outliers, "==")) == 1
                if (horizontal)
                    do.call("points", c(list(x=y[toplot], y=x[toplot], pch=pch[[i]][toplot], col=col[[i]][toplot]), args[namedargs]))
                else
                    do.call("points", c(list(x=x[toplot], y=y[toplot], pch=pch[[i]][toplot], col=col[[i]][toplot]), args[namedargs]))
            }
        }
        if (type[i] == "db") # boxplot behind
            do.call("boxplot", c(list(x=y, at=at[i], add=TRUE, axes=FALSE, border=my.gray, outline=FALSE, horizontal=horizontal), boxplot.pars))
        if (type[i] %in% c("db", "d")) { # dots in front
            if (horizontal)
                do.call("points", c(list(x=y, y=x, pch=pch[[i]], col=col[[i]]), args[namedargs]))
            else
                do.call("points", c(list(x=x, y=y, pch=pch[[i]], col=col[[i]]), args[namedargs]))
        }
        if (mean.line[i]) { # mean line
            if (horizontal)
                do.call("lines", c(list(rep(mean(y), at[i]+Lme, 2)), mean.pars))
            else
                do.call("lines", c(list(at[i]+Lme, rep(mean(y), 2)), mean.pars))
        }
        if (median.line[i]) { # median line
            if (horizontal)
                do.call("lines", c(list(rep(median(y), at[i]+Lme, 2)), median.pars))
            else
                do.call("lines", c(list(at[i]+Lme, rep(median(y), 2)), median.pars))
        }

        out[[i]] <- data.frame(to.plot, col=col[[i]], pch=pch[[i]])
    }

    # add axes
    if (axes) {
        do.call("localAxis", c(list(side=1+horizontal, at=1:ng, labels=names, tcl=0), args[namedargs]))
        do.call("localAxis", c(list(side=2-horizontal), args[namedargs]))
    }
    # optional sample sizes
    if (show.n)
        do.call("localAxis", c(list(side=3+horizontal, at=1:ng, labels=paste("n=", l, sep=""), tcl=0), args[namedargs], list(mgp=c(3,.5,1), las=0)))
    # add bounding box
    if (frame.plot)
        do.call("localBox", args[namedargs])
    # add titles
    if (ann)
    {
        if (horizontal)
            do.call("localTitle", c(list(main=main, sub=sub, xlab=ylab, ylab=xlab), args[namedargs]))
        else
            do.call("localTitle", c(list(main=main, sub=sub, xlab=xlab, ylab=ylab), args[namedargs]))
    }

    invisible(out)
}

tatsukiplot.formula <- function(formula, data=NULL, ..., subset) {
    if (missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.pass
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    ## special handling for col and pch
    args <- list(...)
    n <- nrow(mf)
    # rep as necessary
    col <- if ("col" %in% names(args)) args$col else par("col")
    pch <- if ("pch" %in% names(args)) args$pch else par("pch")
    # pick out these options
    group.col <- if ("group.col" %in% names(args)) args$group.col else FALSE
    group.pch <- if ("group.pch" %in% names(args)) args$group.pch else FALSE
    # reorder if necessary
    if (!group.col) args$col <- unlist(split(rep(col, length.out=n), mf[-response]))
    if (!group.pch) args$pch <- unlist(split(rep(pch, length.out=n), mf[-response]))
    ##
    do.call("tatsukiplot", c(list(split(mf[[response]], mf[-response])), args))
}
