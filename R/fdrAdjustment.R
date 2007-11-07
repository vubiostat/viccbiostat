fdrAdjustment <- function(data, ranks=NULL, method=c('step.up', 'step.down'))
{
    method <- match.arg(method)
    stepUp <- method == 'step.up'
    if (missing(ranks))
        ranks <- rank(data)
    q <- (data * length(data) / ranks)[order(data)]
    q <- .C(fdrAdjustment,
        q = as.double(q),
        as.integer(length(q)),
        as.integer(stepUp))$q[ranks]
    data.frame(rank=ranks, q)
}
