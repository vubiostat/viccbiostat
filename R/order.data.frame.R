order.data.frame <- function(x, ..., na.last=TRUE, decreasing=FALSE) {
    len <- dim(x)[2]
    len2 <- length(na.last)
    len3 <- length(decreasing)
    if (len < len2)
        na.last <- na.last[1 : len]
    if (len > len2)
        na.last[(len2 + 1) : len] <- TRUE
    if (len < len3)
        decreasing <- decreasing[1 : len]
    if (len > len3)
        decreasing[(len3 + 1) : len] <- FALSE
    ox <- 1:dim(x)[1]
    for (i in len:1) {
        ox <- order(x[,i], order(ox) * (1 - 2 * decreasing[i]), na.last=na.last[i], decreasing=decreasing[i])
    }
    return(ox)
}
