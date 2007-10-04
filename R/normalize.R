normalize <- function(x, margin = c(0,1,2), rename = TRUE)
{
    margin <- match.arg(margin)
    if (margin == 0 || is.null(dim(x)))
    {
        result <- (x - mean(x))/sd(x)
    }
    else
    {
        result <- apply(x, margin, normalize)
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
