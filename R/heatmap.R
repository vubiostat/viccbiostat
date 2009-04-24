htmp <- function(data=NULL, group=NULL, main="heatmap", xlab="feature", ylab="sample", col=RColorBrewer::brewer.pal(12,"Set3"), filename=NULL, png.args=NULL, heat.colors=NULL) {

    require('stats') || stop("package 'stats' is missing")
    if (is.null(heat.colors) || !is.null(filename))
        require('grDevices') || stop("package 'grDevices' is missing")

    if (is.data.frame(data))
        data <- as.matrix(data[sapply(data, mode) == "numeric"])
    # get colors by group index
    if (is.factor(group))
        rc <- col[as.integer(group)]
    # assume colors are already defined
    else
        rc <- group

    if (is.null(heat.colors)) {
        i <- seq(0, 1, length.out=256)
        heat.colors <- c(hsv(h=0.9794372, v=i))
        if (min(data) < 0)
            heat.colors <- c(heat.colors, rev(hsv(h=0.5839329, v=i)))
    }

    labrow <- rownames(data)
    if (is.null(labrow) || length(labrow) >= 50)
        labrow <- NA

    labcol <- colnames(data)
    if (is.null(labcol) || length(labcol) >= 50)
        labcol <- NA

    if (!is.null(filename))
        do.call('png', c(filename=filename, png.args))
        #png(filename=filename, width=480, height=480, units="px", pointsize=12, bg="white", res=NA, restoreConsole=TRUE)

    result <- heatmap(data, scale="column", margins=c(5,10),
            RowSideColors=rc, labRow=labrow, labCol=labcol,
            main=main, xlab=xlab, ylab=ylab, col=heat.colors)

    if (!is.null(filename))
        dev.off()

    invisible(result)
}
