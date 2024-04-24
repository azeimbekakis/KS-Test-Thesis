load("power.rda")

n100 <- aggregate(p ~ meth + dep, data = hn.data, FUN = function(x) mean(x < 0.05, na.rm = TRUE))
g100 <- aggregate(p ~ meth + dep, data = hg.data, FUN = function(x) mean(x < 0.05, na.rm = TRUE))

load("power2.rda")

n200 <- aggregate(p ~ meth + dep, data = hn.data, FUN = function(x) mean(x < 0.05, na.rm = TRUE))
g200 <- aggregate(p ~ meth + dep, data = hg.data, FUN = function(x) mean(x < 0.05, na.rm = TRUE))

aa <- cbind(n100, n200[,3])
aa <- aa[, c(2, 1, 3, 4)]
aa <- cbind(aa, g100[,3], g200[,3])
aa$meth <- rep(c("naive", "copula"), 3)

library(xtable)
print(xtable(aa, digits = 3), include.rownames = FALSE)
