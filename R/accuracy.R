accuracy <- function(actual, prediction) {
    if (length(actual) != length(prediction))
        stop("All arguments must have the same length.")
    lvls <- union(levels(as.factor(actual, exclude=NULL)), levels(as.factor(prediction, exclude=NULL)))
    actual <- as.factor(actual, lvls, exclude=NULL)
    prediction <- as.factor(prediction, lvls, exclude=NULL)
    tmp <- table(actual, prediction, exclude=NULL)
    result <- diag(tmp) / rowSums(tmp)
    if (any(is.na(lvls))) {
        result <- result[-which(is.na(lvls))]
    }
    result <- c(overall = sum(diag(tmp)) / sum(tmp), result)
    result[is.nan(result)] <- 0
    result
}
