vpsm <- function() {
    if (!exists('script')) {
        load("mcmc.RData")
        outfile <- 'psm.RData'
    }
    require(runjags)
    fit <- convert(jagsfit)
    R2WinBUGS:::attach.bugs(fit)
    pm <- ps <- NULL

                                        # sigma
    pm$sigma <- apply(sigma, 2, median)
    ps$sigma <- apply(sigma, 2, mad)

                                        # alpha
    pm$alpha <- apply(alpha, 2, median)
    ps$alpha <- apply(alpha, 2, mad)
    names(pm$alpha) <- names(ps$alpha) <- rownames(Vector)

                                        # beta
    pm$beta <- apply(beta, 2, median)
    ps$beta <- apply(beta, 2, mad)
    names(pm$beta) <- names(ps$beta) <- rownames(Vector)

                                        # a,b
    pm$a <- apply(a, 2, median)
    pm$b <- apply(b, 2, median)
    ps$a <- apply(a, 2, mad)
    ps$b <- apply(b, 2, mad)

                                        # omega
    pm$omega <- apply(omega, 2, median)
    ps$omega <- apply(omega, 2, mad)

                                        # Export
    save(pm, ps, thresh, file=outfile)
}
