QC <- function(x, group=NULL, methods=c("pearson", "spearman", "kendall", "kappa", "icc", "cv"), cutoff=0, fixedEffect=1, randomEffect=NULL, silent=FALSE) {

    methods <- match.arg(methods, several.ok=TRUE)
    nrow <- nrow(x)

    if (is.null(group)) { # not within group

        # calculate average of whole data set {{{1
        CalAvg <- function(vec) {
            (sum(vec) - 1) / (length(vec) - 1)
        }

        dat <- data.frame(row.names=rownames(x))

        for (method in methods) {

            tmp <- list()

            if (method %in% c("pearson", "spearman", "kendall")) {

                tmp <- data.frame(apply(cor(t(x), method=method), 2, CalAvg))
                colnames(tmp) <- method

            } else if (method == "kappa") {

                if (require(e1071)) {

                    x1 <- ifelse(x > cutoff, 1, 0)
                    a <- numeric(nrow)
                    for (i in seq(n)) {
                        b <- numeric(nrow)
                        for (j in seq(nrow)) {
                            b[j] <- classAgreement(table(x1[i,], x1[j,]))$kappa
                        }
                        a[i] <- CalAvg(b)
                    }
                    tmp <- data.frame(kappa=a)

                } else if (!silent) {
                    warning("package 'e1071' is missing (required for 'kappa')")
                }

            } else if (method == "icc") {

                if (require(nlme)) {

                    if (is.null(rand) || is.null(fixed)) {
                        warning("Please provide random and fixed effects")
                        next
                    }

                    #randomEffect <- as.factor(randomEffect)
                    #fixedEffect <- as.factor(fixedEffect)

                    a <- matrix(numeric(2*nrow), ncol=2)
                    for (i in seq(nrow)) {
                        fml <- try(lme(x[i,] ~ fixedEffect, random= ~1 | randomEffect), silent=TRUE)
                        if (class(fml) != "try-error")
                            a <- as.numeric(VarCorr(fml)[,1])
                    }
                    tmp <- data.frame(icc=(a[,1] / (a[,1] + a[,2])))

                } else if (!silent) {
                    warning("package 'nlme' is missing (required for 'icc')")
                }

            } else if (method == "cv") {

                a <- apply(x, 1, sd)
                b <- apply(x, 1, mean)
                c <- a / b
                tmp <- data.frame(sd=a, mean=b, cv=c)

            }

            dat <- cbind(dat, tmp)
        }

        return(dat)

    } else { # within group

        if (nrow != length(group)) stop("group and data are not the same size")

        group <- as.factor(group)
        nlvl <- nlevels(group)

        if (is.null(randomEffect))
            rel <- rep(list(NULL, nlvl))
        else
            rel <- split(randomEffect, group)

        dat <- mapply(QC, x=split(x, group),
                      randomEffect=rel,
                      silent=c(silent, rep(TRUE, nlvl-1)),
                      MoreArgs=list(group=NULL,
                                    methods=methods,
                                    cutoff=cutoff,
                                    fixedEffect=fixedEffect),
                      SIMPLIFY=FALSE)
        # Manual unsplit
        x <- dat[[1]][rep(NA, nrow),, drop=FALSE]
        rownames(x) <- unsplit(lapply(dat, rownames), group)
        split(x, group) <- dat
        # Manual unsplit
        return(x)
    }
}

