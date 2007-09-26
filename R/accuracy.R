accuracy <- function(known, predict)
{
    lvls1 <- levels(factor(known))
    lvls2 <- levels(factor(predict))
    lvls <- union(lvls1, lvls2)
    known <- factor(known, lvls)
    predict <- factor(predict, lvls)
    if (length(known) != length(predict))
    {
        stop("All arguments must have the same length.")
    }
    tmp <- table(known, predict)
    result <- c(overall = (tmp[1,1] + tmp[2,2]) / sum(tmp))
    for (i in 1:length(lvls))
    {
        result <- c(result, tmp[i,i] / sum(tmp[i,]))
        names(result)[i+1] <- lvls[i]
    }
    result[is.nan(result)] <- 0
    return(result)
}
