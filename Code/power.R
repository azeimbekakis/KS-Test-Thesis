source("tacvfARMA.R")
source("ksfitted.R")
library("truncdist")

n <- 100
nrep <- 1000

n.par <- c(mean = 8, sd = sqrt(8))
g.par <- c(shape = 8, rate = 1)

genData <- function(n, phi, theta, qdist) {
    x <- arima.sim(model = list(ar = phi, ma = theta), n = n)
    sigma <- sqrt(tacvfARMA.my(phi = phi, theta = - theta, maxLag = 0))
    qdist(pnorm(x, sd = sigma))
}

do1rep <- function(n, phi, theta, qdist, hypod, start) {
    x <- genData(n, phi, theta, qdist)
    ks.f <- ks.test.fitted(x, hypod, fit = TRUE, serial = FALSE, param = start)
    ks.fs <- ks.test.fitted(x, hypod, fit = TRUE, serial = TRUE, param = start)
    c(ks.f$p.value, ks.fs$p.value)
}

## true distribution is truncated normal(8, 8)
qdist <- function(p) qtrunc(p, "norm", a = 0, mean = 8, sd = sqrt(8))

hg.n3 <- t(replicate(nrep, do1rep(n, -0.3, numeric(0), qdist, "gamma", g.par)))
hg.0 <- t(replicate(nrep, do1rep(n, numeric(0), numeric(0), qdist, "gamma", g.par)))
hg.3 <- t(replicate(nrep, do1rep(n, 0.3, numeric(0), qdist, "gamma", g.par)))
                    
        
hg.data <- data.frame(p = c(hg.n3, hg.0, hg.3),
                      meth = gl(2, nrep, nrep * 6, c("not serial", "serial")),
                      dep = gl(3, nrep * 2, nrep * 6, c(-0.3, 0, 0.3)))


## true distribution is gamma(8, 1)
qdist <- function(p) qgamma(p, shape = 8, rate = 1)

hn.n3 <- t(replicate(nrep, do1rep(n, -0.3, numeric(0), qdist, "norm", n.par)))
hn.0 <- t(replicate(nrep, do1rep(n, numeric(0), numeric(0), qdist, "norm", n.par)))
hn.3 <- t(replicate(nrep, do1rep(n, 0.3, numeric(0), qdist, "norm", n.par)))
                    
        
hn.data <- data.frame(p = c(hn.n3, hn.0, hn.3),
                      meth = gl(2, nrep, nrep * 6, c("not serial", "serial")),
                      dep = gl(3, nrep * 2, nrep * 6, c(-0.3, 0, 0.3)))

save(hg.data, hn.data, file = "power.rda")
