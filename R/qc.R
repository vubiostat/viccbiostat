QC <- function(x, group=NULL, methods=c("pearson", "spearman", "kendall", "kappa", "ICC", "CV"), cutoff=0, fixedEffect=1, randomEffect=NULL) {

    method <- match.arg(method, several.ok=TRUE)

    ### Wrapper functions {{{1

    # calculate average of whole data set {{{2
    CalAvg <- function(vec) {
        (sum(vec) - 1) / (length(vec) - 1)
    }

    # correlation {{{2
    Cor <- function(x, method) {
        tmp <- data.frame(apply(cor(t(x), method=method), 2, CalAvg))
        colnames(tmp) <- method
        tmp
    }

    # CV {{{2
    CV <- function(x) {
        a <- apply(x, 1, sd)
        b <- apply(x, 1, mean)
        c <- a / b
        data.frame(sd=a, mean=b, cv=c)
    }

    # ICC {{{2
    ICC <- function(x, fixedEffect, randomEffect) {
        n <- nrow(x)
        a <- matrix(numeric(2*n), ncol=2)
        for (i in seq(n)) {
            fml <- try(lme(x[i,] ~ fixedEffect, random= ~ 1 | randomEffect), silent=TRUE)
            if (class(fml) != "try-error")
                a <- as.numeric(VarCorr(fml)[,1])
        }
        data.frame(icc=(a[,1] / (a[,1] + a[,2])))
    }

    # kappa {{{2
    Kappa <- function(x, cutoff) {
        n <- nrow(x)
        x <- ifelse(x > cutoff, 1, 0)
        a <- numeric(n)
        for (i in seq(n)) {
            b <- numeric(n)
            for (j in seq(n)) {
                b[j] <- classAgreement(table(x[i,], x[j,]))$kappa
            }
            a[i] <- CalAvg(res)
        }
        data.frame(kappa=a)
    }

    ### }}}

    nrow <- nrow(x)
    ncol <- ncol(x)

    if (is.null(group)) { # not within group

        for (method in methods) {
            dat <- list()

            if (method %in% c("pearson", "spearman", "kendall")) {

                dat <- cbind(dat, Cor(x, method=method))

            } else if (method == "kappa") {

                if (require(e1071)) {

                    dat <- cbind(dat, Kappa(x, cutoff=cutoff))

                } else {
                    warning("package 'e1071' is missing (required for 'kappa')")
                }

            } else if (method == "ICC") {

                if (require(nlme)) {

                    if (is.null(rand) || is.null(fixed)) {
                        warning("Please provide random and fixed effects")
                        next
                    }

                    randomEffect <- as.factor(randomEffect)
                    fixedEffect <- as.factor(fixedEffect)

                    dat <- cbind(dat, ICC(x, fixedEffect=fixedEffect, randomEffect=randomEffect))

                } else {
                    warning("package 'nlme' is missing (required for 'ICC')")
                }

            } else if (method == "CV") {

                dat <- cbind(dat, CV(x, cutoff=cutoff))

            }

        }

    } else { # within group

        group <- as.factor(group)
        lvls <- levels(group)
        nlvl <- length(lvls)

        if (nrow != length(group)) stop("group and data are not the same size")

        xl <- split(x, group)

        for (method in methods) {
            dat <- list()

            if (method %in% c("pearson", "spearman", "kendall")) {

                dat <- cbind(dat, unsplit(lapply(xl, Cor, method=method), group))

            } else if (method == "kappa") {

                if (require(e1071)) {

                    dat <- cbind(dat, unsplit(lapply(xl, Kappa, cutoff=cutoff), group))

                } else {
                    warning("package 'e1071' is missing (required for 'kappa')")
                }

            } else if (method == "ICC") {

                if (require(nlme)) {

                    randomEffect <- as.factor(randomEffect)
                    fixedEffect <- as.factor(fixedEffect)

                    dat <- cbind(dat, unsplit(mapply(ICC, xl, list(fixedEffect), split(randomEffect, group)), group))

                } else {
                    warning("package 'nlme' is missing (required for 'ICC')")
                }

            } else if (method == "CV") {

                dat <- cbind(dat, unsplit(lapply(xl, CV), group))

            }
        }
    }

    return(dat)
}

