##' Distribution Functions with Given Parameters
##'
##' Generate a (d, p, q, or r) function of a distribution
##'
##' This function creates a function for a distribution with given parameters
##' and a desired usage (d, p, q, or r).
##'
##' @param dist A string specificing the distribution, assuming function names
##' prefixed with (d, p, q, or r) are available.
##' @param param A named vector specifying the parameters of the distribution
##' @param fn A character of `d`, `p`, `q`, or `r` for density, distribution,
##' quantile, and random number generation.
##' @return A function with a single argument while other parameters specified
##' through the input `param`.
##' 
fitteddist <- function(dist, param, fn) {
    nm <- names(param); l <- length(param)
    fun <- get(paste0(fn, dist))
    f <- formals(fun)
    args <- names(f)
    m <- match(nm, args)
    formals(fun) <- c(f[c(1, m)], f[-c(1, m)])
    myfun <- function(z) {}
    body(myfun) <- parse(
        text = paste("fun(z,", paste0("param[", 1L:l, "]", collapse = ", "), ")"))
    myfun
}


##' Kolmogorov-Smirnov Tests with Fitted Parameters and Serial Dependence
##'
##' Performs a one-sample K-S test with fitted parameters and serial dependence
##'
##' This function allows fitted parameters through maximim likelihood when
##' performing K-S test for commonly used distributions via parametric
##' bootstrap. Serial dependence in allowed and preserved in bootstrap by a
##' auto-selected working ARMA model for the residuals on the normal scale.
##' 
##' @param x a numeric vector of data values.
##' @param dist a string of the name of the hypothesized distribution
##' @param B number of bootstrap replicates
##' @param fit if TRUE, test performed at the fitted parameter
##' @param serial if TRUE, account for serial dependence via an auto-selected
##' working ARMA model
##' @param param a named vector for the hypothesized distribution. If fit ==
##' TRUE, it will be used as the starting value for fitting
ks.test.fitted <- function(x, dist, B = 1000, fit = TRUE, serial = FALSE,
                         param = c()) {
    ## if estimation is needed, fit marginal distribution
    ddist <- get(paste0("d", dist))
    if (fit) param  <- MASS::fitdistr(x, ddist, start = split(param, names(param)))$estimate
    pdist <- fitteddist(dist, param, "p")
    rdist <- fitteddist(dist, param, "r")
    ## get observed ks-stat
    stat <- ks.test(x, pdist)$statistic
    stat.b <- double(B)
    n <- length(x)
    ## working, sigma, qdist are only used when serial == TRUE
    if (serial) {
        z <- qnorm(pdist(x))
        fit.arma <- forecast::auto.arima(z, ic = "aic", max.d = 0, max.D = 0, allowmean = FALSE)
        ## working arma model to preserve serial dependence
        working <- list(ar = fit.arma$model$phi, ma = fit.arma$model$theta)
        sigma <- sqrt(tacvfARMA.my(phi = working$ar, theta = - working$ma, maxLag = 0))     
        qdist <- fitteddist(dist, param, "q")
    }
    ## bootstrapping
    for (i in 1:B) {
        if (serial)
            x.b <- qdist(pnorm(stats::arima.sim(working, n = n), sd = sigma))
        else x.b <- rdist(n)
        if (fit) {
            fitted.b <- MASS::fitdistr(x.b, ddist, start = split(param, names(param)))
            pdist.b <- fitteddist(dist, fitted.b$estimate, "p")
            stat.b[i] <- ks.test(x.b, pdist.b)$statistic
        } else stat.b[i] <- ks.test(x.b, pdist)$statistic
    }
    p.value <- (sum(stat.b >= stat) + 0.5) / (B + 1)
    list(stat = stat, p.value = p.value, stat.b = stat.b)
}
