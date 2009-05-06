wga.test <- function(x, ...) UseMethod("wga.test")

wga.test.default <- function(x, y, ...) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    n1 <- length(x)
    n2 <- length(y)
    if (n1 < 2)
        stop("Not enough x observations.")
    if (n2 < 2)
        stop("Not enough y observations.")
    t1 <- (n1 * (n1 - 1)) / 2
    t2 <- (n2 * (n2 - 1)) / 2
    dw1 <- sum(dist(x))
    dw2 <- sum(dist(y))
    dB <- abs(mean(x) - mean(y))
    denom <- (dw1 + dw2) / (t1 + t2)
    wga <- if (dB == 0)
        0
    else
        dB / denom
    wga
}

wga.test.formula <- function(formula, data, subset, na.action, ...) {
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
    y <- do.call("wga.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
