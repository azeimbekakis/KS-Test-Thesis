## library(ggplot2)
library(qqplotr)

source("tacvfARMA.R")
source("ksfitted.R")

n <- 200
nrep <- 1000

### Section 2: fitted distribution

do1rep <- function(n, dist, param) {
    rdist <- getfndist(dist, param, "r")
    x <- rdist(n)
    ks <- ks.test.fitted(x, dist, param = param, fit = TRUE, serial = FALSE)
    c(ks$p.naive, ks$p.value)
}

## normal
n.par <- c(mean = 8, sd = sqrt(8))
# n.f <- t(replicate(nrep, do1rep(n, "norm", n.par)))


## gamma
g.par <- c(shape = 8, rate = 1)
# g.f <- t(replicate(nrep, do1rep(n, "gamma", g.par)))


## f.data <- data.frame(p = c(n.f, g.f),
##                      dist = gl(2, nrep * 2, nrep * 4, c("normal", "gamma")),
##                      meth = gl(2, nrep, nrep * 4, c("naive", "bootstrap")))

save(f.data, file = "f.rda")

### Section 3: serial dependence (but known distribution)

genData <- function(n, phi, theta, qdist) {
    x <- arima.sim(model = list(ar = phi, ma = theta), n = n)
    sigma <- sqrt(tacvfARMA.my(phi = phi, theta = - theta, maxLag = 0))
    qdist(pnorm(x, sd = sigma))
}

do1rep <- function(n, phi, dist, param) {
    rdist <- getfndist(dist, param, "r")
    qdist <- getfndist(dist, param, "q")
    pdist <- getfndist(dist, param, "p")
    x <- genData(n,numeric(0), phi, qdist)
    ks.test(x, pdist)$p.value
}

n.s.8 <- replicate(nrep, do1rep(n, 0.8, "norm", n.par))
n.s.4 <- replicate(nrep, do1rep(n, 0.4, "norm", n.par))
n.s.n4 <- replicate(nrep, do1rep(n, -.4, "norm", n.par))
n.s.n8 <- replicate(nrep, do1rep(n, -.8, "norm", n.par))


g.s.8 <- replicate(nrep, do1rep(n, 0.8, "gamma", n.par))
g.s.4 <- replicate(nrep, do1rep(n, 0.4, "gamma", n.par))
g.s.n4 <- replicate(nrep, do1rep(n, -.4, "gamma", n.par))
g.s.n8 <- replicate(nrep, do1rep(n, -.8, "gamma", n.par))



s.data <- data.frame(p = c(n.s.n8, n.s.n4, n.s.4, n.s.8,
                           g.s.n8, g.s.n4, g.s.4, g.s.8),
                     dist = gl(2, nrep * 4, nrep * 8, c("normal", "gamma")),
                     rho = gl(4, nrep, nrep * 8, c(-0.8, -0.4, 0.4, 0.8)))

save(s.data, file = "sim-ma.rda")
