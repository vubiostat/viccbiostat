wilcoxon.test <- function(x, ...) UseMethod("wilcoxon.test")

wilcoxon.test.default <- function(x, y, ...) {
    n1 <- length(x)
    n2 <- length(y)
    result <- wilcox.test(x, y, ...)
    result$z <- (result$statistic - n1 * n2 / 2) / sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
    result
}

wilcoxon.test.formula <- function(formula, data, subset, na.action, ...) {
    if (missing(formula)
        || (length(formula) != 3)
        || (length(attr(terms(formula[-2]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- as.factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("wilcoxon.test", c(DATA, list(...)))
    y$data.name <- DNAME
    if(length(y$estimate) == 2)
        names(y$estimate) <- paste("mean in group", levels(g))
    y
}
