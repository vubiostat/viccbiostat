info.test <- function(x, ...) UseMethod("info.test")

info.test.default <- function(x, y, ...) {
    nx <- length(x)
    ny <- length(y)
    if (nx < 1)
        stop("not enough 'x' observations")
    if (ny < 1)
        stop("not enough 'y' observations")
    d <- c(x, y)
    grp <- c(rep(1, nx), rep(2, ny))
    num <- length(d)
    # A is for UPUP, B is for UPDOWN
    A <- grp[order(d, grp)]
    B <- grp[order(d,-grp)]
    info <- .C("_infotest",
        as.integer(A),
        as.integer(B),
        as.double(num),
        as.double(nx),
        info = as.double(1)
    )$info
    info
}

info.test.formula <- function(formula, data, subset, na.action, ...) {
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
    y <- do.call("info.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
