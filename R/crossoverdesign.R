crossoverdesign <- function(s, t, permutations=FALSE) {
    if (s < 1)
        stop("Need at least 1 subject.")
    if (t < 2)
        stop("Need at least 2 treatments.")
    if (t > 10 && permutations) {
        permutations <- FALSE
        warning("Using permutations=FALSE since t > 10.")
    }
    t1 <- t - 1
    sv <- 1:s
    m <- matrix(rep(1:t, times=t) + rep(1:t, each=t) - 2, t) %% t + 1
    # the ordering of samples within each block
    entries <- replicate(ceiling(s/t), sample(t))[sv]
    if (permutations && require(combinat)) {
        # use the permutations set of block permutations
        mats <- lapply(permn(t1), function(x) m[,c(1, x+1)])
        matf <- function(b, e) { mats[[b]][e,] }
        t1f <- factorial(t1)
        tf <- t1f * t
        n <- min(ceiling(s/t), t1f)
        blocks <- replicate(ceiling(s/tf), rep(sample(t1f,n), each=t))[sv]
    } else {
        # use randomly reordered blocks
        blocks <- rep(replicate(ceiling(s/t), sample(t), FALSE), each=t)[sv]
        matf <- { function(b, e) m[e,b] }
    }
    t(mapply(matf, blocks, entries))
}
