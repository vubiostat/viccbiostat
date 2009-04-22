htmp <- function(data=NULL, info=NULL, smp=NULL, grp=NULL, main="heatmap", xlab="gene", ylab="patient", col=cm.colors(256), filename=NULL) {
    if (is.null(data) || is.null(info))
        stop("Please provid both of data set and information set.")
    if (nrow(data) != nrow(info))
        stop("Data set and information set should have same number of samples.")

    data <- as.matrix(data)
    l <- ncol(data)
    cc <- rainbow(l, start=0, end=max(1,l-1)/l)
    rc <- palette()[as.integer(factor(info[,grp]))+1]

    if (length(rc) >=50)
        labrow = NA
    else
        labrow = info$smp

    if (length(cc) >= 50 )
        labcol = NA
    else
        labcol = colnames(data)

    if (!is.null(filename))
        png(filename=filename, width=480, height=480, units="px", pointsize=12, bg="white", res=NA, restoreConsole=TRUE)

    heatmap(data,
            scale="column",
            RowSideColors=rc,
            ColSideColors=cc,
            margins=c(5,10),
            labRow=labrow,
            labCol=labcol,
            main=main,
            xlab=xlab,
            ylab=ylab,
            col=col)

    if (!is.null(filename))
        dev.off()
}
