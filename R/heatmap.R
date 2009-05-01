htmp <- function(data, group, main="cluster", xlab="features", ylab="samples", group.col=RColorBrewer::brewer.pal(12,"Set3"), filename=NULL, png.args=NULL, heat.colors=NULL) {

    require('stats') || stop("package 'stats' is missing")
    if (is.null(heat.colors) || !is.null(filename))
        require('grDevices') || stop("package 'grDevices' is missing")

    if (length(group) != nrow(data))
        stop("length(group) and nrow(data) do not match")

    if (is.data.frame(data))
        data <- as.matrix(data[sapply(data, mode) == "numeric"])
    # assume colors are already defined
    if (is.character(group))
        rc <- group
    # get colors by group index
    else
        rc <- group.col[as.integer(group)]

    # make our own colors
    if (is.null(heat.colors)) {
        mx <- max(data)
        mn <- min(data)
        i <- abs(c(mn,mx))
        i <- round(i / max(i), 2)
        heat.colors <- NULL
        if (mn < 0)
            heat.colors <- c(heat.colors, hsv(h=0.6, v=seq(i[1], 0.1, -0.01)))
        heat.colors <- c(heat.colors, "#000000")
        if (mx > 0)
            heat.colors <- c(heat.colors, hsv(h=1, v=seq(0.1, i[2], 0.01)))
    }

    labrow <- rownames(data)
    if (is.null(labrow) || length(labrow) >= 50)
        labrow <- NA

    labcol <- colnames(data)
    if (is.null(labcol) || length(labcol) >= 50)
        labcol <- NA

    if (!is.null(filename))
        do.call(png, c(filename=filename, png.args))
        #png(filename=filename, width=480, height=480, units="px", pointsize=12, bg="white", res=NA, restoreConsole=TRUE)

    result <- heatmap(data, scale="none", margins=c(5,10),
                      RowSideColors=rc, labRow=labrow, labCol=labcol,
                      main=main, xlab=xlab, ylab=ylab, col=heat.colors)

    if (!is.null(filename))
        dev.off()

    invisible(result)
}
