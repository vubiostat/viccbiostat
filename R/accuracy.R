accuracy <- function(known, predict) {
    if (length(known) != length(predict))
        stop("All arguments must have the same length.")
    lvls <- union(levels(factor(known)), levels(factor(predict)))
    known <- factor(known, lvls)
    predict <- factor(predict, lvls)
    tmp <- table(known, predict, exclude=NULL)
    result <- c(overall = (tmp[1,1] + tmp[2,2]) / sum(tmp))
    for (i in 1:length(lvls)) {
        result <- c(result, tmp[i,i] / sum(tmp[i,]))
        names(result)[i+1] <- lvls[i]
    }
    result[is.nan(result)] <- 0
    return(result)
}
