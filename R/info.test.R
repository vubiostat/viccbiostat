info.test <- function(x, y, ...) {
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
    .C(infotest,
        as.integer(A),
        as.integer(B),
        as.double(num),
        as.double(nx),
        info = as.double(1)
    )$info
}
