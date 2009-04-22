htmp <- function(dat=NULL, info=NULL, smp=NULL, grp=NULL, tle="heatmap", labx="gene", laby="patient", autosave=FALSE) {
    if (is.null(dat) || is.null(info))
        stop("Please provid both of data set and information set.")
    if (dim(dat)[1] != dim(info)[1])
        stop("Data set and information set should have same numer of patients.")

    dat <- as.matrix(dat)
    l <- dim(dat)[2]
    cc <- rainbow(l, start=0, end=max(1,l-1)/l)
    rc <- palette()[as.integer(factor(info[,grp]))+1]

    if (length(rc) >=50)
        labrow = NA
    else
        labrow = info$smp

    if (length(cc) >= 50 )
        labcol = NA
    else
        labcol = colnames(dat)

    if (autosave)
        png(filename = "heatmap.png", width = 480, height = 480,
            units = "px", pointsize = 12, bg = "white", res = NA,
            restoreConsole = TRUE)

    heatmap(dat,
            col = cm.colors(256),
            scale="column",
            RowSideColors = rc,
            ColSideColors = cc,
            margins=c(5,10),
            labRow = labrow,
            labCol = labcol,
            xlab = labx,
            ylab= laby,
            main = tle)

    if (autosave) dev.off()
}
