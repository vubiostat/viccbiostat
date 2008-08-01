fdrAdjustment <- function(data, ranks=NULL, method=c("step.up", "step.down"), c=NULL) {
    method <- match.arg(method)
    stepUp <- method == "step.up"
    if (is.null(c))
        c <- log(length(data)) - digamma(1)
    if (missing(ranks))
        ranks <- rank(data)
    q <- (data * length(data) * c / ranks)[order(data)]
    q <- .C("_fdrAdjustment",
        q = as.double(q),
        as.integer(length(q)),
        as.integer(stepUp))$q[ranks]
    data.frame(rank=ranks, q)
}
