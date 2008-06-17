normalize <- function(x, margin = 0, rename = TRUE, na.rm = TRUE) {
    margin <- as.numeric(match.arg(as.character(margin), c('0', '1', '2')))
    if (margin == 0 || is.null(dim(x)))
        result <- (x - mean(x, na.rm=na.rm))/sd(x, na.rm=na.rm)
    else {
        result <- apply(x, margin, function(x) (x - mean(x, na.rm=na.rm))/sd(x, na.rm=na.rm))
        if (margin == 1)
            result <- t(result)
        if (!identical(dim(result), dim(x)))
            dim(result) <- dim(x)
        dimnames(result) <- dimnames(x)
        if (rename)
            dimnames(result)[[margin]] <- paste(dimnames(result)[[margin]], "std", sep=".")
    }
    return(result)
}
