library(qqplotr)

load("sim.rda")

## Section 2: Fitted parameters
gg.f <- ggplot(data = f.data, mapping = aes(sample = p)) +
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .3) +
    facet_grid(vars(dist), vars(meth)) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)

ggsave(filename = "pp_fit.pdf", plot = gg.f, path = "../Manuscript", height = 4, width = 4)


## Section 3: Demonstrate the consequence of serial dependence
gg.s <- ggplot(data = s.data, mapping = aes(sample = p)) +
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .3) +
    facet_grid(vars(dist), vars(rho)) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)

ggsave(filename = "pp_s.pdf", plot = gg.s, path = "../Manuscript", height = 3, width = 6)

## corrected for serial dependence
gg.ss <- ggplot(data = subset(ss.data, meth == "bootstrap"),
             mapping = aes(sample = p)) +
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .3) +
    facet_grid(vars(dist), vars(dep)) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)

ggsave(filename = "pp_ss.pdf", plot = gg.ss, path = "../Manuscript", height = 3, width = 6)

## Section 4: fitted parameter and serial dependence
gg.ssf <- ggplot(data = subset(ss.data, meth == "bootstrap"),
                 mapping = aes(sample = p)) +
    stat_pp_band(distribution = "unif") +
    stat_pp_line() +
    stat_pp_point(distribution = "unif", cex = .3) +
    facet_grid(vars(dist), vars(dep)) +
    labs(x = "Probability Points", y = "Cumulative Probability") +
    coord_fixed() # theme(aspect.ratio=1)

ggsave(filename = "pp_ssf.pdf", plot = gg.ssf, path = "../Manuscript", height = 3, width = 6)
