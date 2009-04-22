heat<-function(data){
    x <- as.matrix(data)
    d <- ncol(x[,c(2:8)])
    cc <- rainbow(d, start=0, end=0.3)
    hv <- heatmap(x[, c(2:8)], col=cm.colors(256), scale="column",
        RowSideColors=palette()[ x[, "group"]+2 ],
        ColSideColors=cc,
        margins=c(5,10),
        xlab="winners ", ylab="Patients",
        main="heatmap")
}
