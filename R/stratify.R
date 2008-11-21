stratify <- function(..., n, RANDOMIZE=TRUE, drop=FALSE, sep=".", lex.order=FALSE) {
    fs <- list(...)
    f <- unclass(interaction(fs, drop=drop, sep=sep, lex.order=lex.order))
    resample <- if (RANDOMIZE) {
        function(x) { sample(list(x))[[1]] }
    } else {
        function(x) { x }
    }
    f1 <- function(x) { rbind(x[1,], apply(x, 2, diff)) }
    f2 <- function(x) { resample(rep(1:n, x)) }
    unlist(apply(f1(round(apply(matrix(rep(table(f)/n, each=n), nrow=n), 2, cumsum))), 2, f2))[rank(f, ties.method="first")]
    #print(t <- table(f)/n)
    #print(m <- round(apply(matrix(rep(t, each=n), nrow=n), 2, cumsum)))
    #print(m <- f1(m))
    #print(x <- unlist(apply(m, 2, f2)))
    #print(length(x))
    #x[rank(f, ties.method="first")]
}
