accuracy <- function(actual, prediction) {
    if (length(actual) != length(prediction))
        stop("All arguments must have the same length.")
    lvls <- union(levels(factor(actual, exclude=NULL)), levels(factor(prediction, exclude=NULL)))
    actual <- factor(actual, lvls, exclude=NULL)
    prediction <- factor(prediction, lvls, exclude=NULL)
    tmp <- table(actual, prediction, exclude=NULL)
    result <- diag(tmp) / rowSums(tmp)
    if (any(is.na(names(result)))) {
        result <- result[-which(is.na(names(result)))]
    }
    result <- c(overall = sum(diag(tmp)) / sum(tmp), result)
    result[is.nan(result)] <- 0
    result
}
