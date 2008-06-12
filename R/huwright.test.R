.loglkd <- function(logmu, x, beta0, beta1) {
    logsigma2 <- beta0 + beta1 * logmu
    loglike <- sum(1 / 2 * logsigma2 + (x - exp(logmu)) ^ 2 / (2 * exp(logsigma2)))
    return(loglike)
}

.loglkdbeta <- function(beta, x, logmu) {
    logsigma2 <- beta[1] + beta[2] * logmu
    loglikei <- 1 / 2 * logsigma2 + (x - exp(logmu)) ^ 2 / (2 * exp(logsigma2))
    loglike <- sum(loglikei)
    return(loglike)
}

.getmusigma2mle <- function(x, tol) {
    m <- dim(x)[1]
    n <- dim(x)[2]
    v <- var(log(rchisq(10 ^ 6,  n - 1)))
    
    X.vector <- na.exclude(as.vector(x))
    smallvalue <- quantile(X.vector[X.vector >= 0], 0.01)
    x[x < 0] <- smallvalue
    
    xmean <- apply(x, 1, mean, na.rm = T)
    xvar <- apply(x, 1, var, na.rm = T)
    xvar[(!is.na(xmean)) & (is.na(xvar))] <- 0
    
    idx <- (1:m)[!(is.na(xmean) | xvar == 0)]
    lsfitobj <- lsfit(log(xmean[idx]), log(xvar[idx]))
    xbetahat <- lsfitobj$coef[1:2]
    temp <- lsfitobj$residuals
    xd2hat <- var(temp) - v
    xinitial <- c(log(xmean), xbetahat)
    logxtemp.var <- log(xvar)
    lkdsumsloop <- NULL
    for (sloop in 1:3) {
        base <- xinitial
        beta0 <- xinitial[m + 1]
        beta1 <- xinitial[m + 2]
        lkdinitial <- NULL
        for (i in idx) {
            nlmoutput <- nlm(.loglkd, base[i], steptol = 1e-3, iterlim = 20, x = x[i, ], beta0 = beta0, beta1 = beta1)
            xinitial[i] <- nlmoutput$estimate
            lkdinitial <- c(lkdinitial, nlmoutput$minimum)
        }
        betaprev <- c(beta0, beta1)
        xinitial[(m + 1):(m + 2)] <- nlm(.loglkdbeta, betaprev, steptol = 1e-3, iterlim = 20, x = x[idx, ], logmu = xinitial[idx])$estimate
        lkdsumsloop <- c(lkdsumsloop, 0 - sum(lkdinitial))
    }
    lkdinf <- lkdsumsloop[1] + 1 / (1 - (lkdsumsloop[3] - lkdsumsloop[2]) / (lkdsumsloop[2] - lkdsumsloop[1])) * (lkdsumsloop[2] - lkdsumsloop[1])
    if ((lkdinf - lkdsumsloop[3]) >= tol) {
        iter <- 0
        repeat {
            lkdsumsloop <- lkdsumsloop[ - 1]
            base <- xinitial
            beta0 <- xinitial[m + 1]
            beta1 <- xinitial[m + 2]
            lkdinitial <- NULL
            for (i in idx) {
                nlmoutput <- nlm(.loglkd, base[i], steptol = 1e-3, iterlim = 20, x = x[i, ], beta0 = beta0, beta1 = beta1)
                xinitial[i] <- nlmoutput$estimate
                lkdinitial <- c(lkdinitial, nlmoutput$minimum)
            }
            betaprev <- c(beta0, beta1)
            xinitial[(m + 1):(m + 2)] <- nlm(.loglkdbeta, betaprev, steptol = 1e-3,  iterlim = 20, x = x[idx, ], logmu = xinitial[idx])$estimate
            lkdsumsloop <- c(lkdsumsloop, 0 - sum(lkdinitial))
            lkdinf <- lkdsumsloop[1] + 1 / (1 - (lkdsumsloop[3] - lkdsumsloop[2]) / (lkdsumsloop[2] - lkdsumsloop[1])) * (lkdsumsloop[2] - lkdsumsloop[1])
            iter <- iter + 1
            if ((lkdinf - lkdsumsloop[3]) < tol | iter == 10)
                break
        }
    }
    xbeta.mle <- xinitial[(m + 1):(m + 2)]
    xbar.mle <- exp(xinitial[1:m])
    xs2.mle <- exp(xbeta.mle[1]) * xbar.mle ^ xbeta.mle[2]
    output <- list(xbar.mle,  xs2.mle,  xbeta.mle,  xd2hat)
    names(output) <- c("xbar.mle", "xs2.mle", "xbeta.mle", "xd2hat")
    return (output)
}

huwright.test <- function(x, y, tol=1, ...) {
    temp1 <- .getmusigma2mle(t(x), tol)
    mu1.mle <- temp1$xbar.mle      
    var1.mle <- temp1$xs2.mle      
    temp2 <- .getmusigma2mle(t(y), tol)
    mu2.mle <- temp2$xbar.mle      
    var2.mle <- temp2$xs2.mle      
    delta <- (mu1.mle - mu2.mle) / (sqrt((var1.mle + var2.mle) / 6))
    return(abs(delta))
}
