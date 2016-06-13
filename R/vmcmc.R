vmcmc <- function() {
                                        # Setup
    debug <- if (grepl('neon', Sys.info()['nodename'])) FALSE else TRUE
    model <- 'vModel.jag'
    if (!exists('script')) {
        thresh <- 0
        load("data/vector-ss.RData")
        outfile <- 'mcmc.RData'
    }

                                        # Filter low/zero counts
    applyFilter(thresh)
    vtr <- apply(Vector, 2:3, sum)

    ## For debugging purposes
    if (debug) {
        ind <- sample(1:dim(Vector)[1], 20)
        Vector <- Vector[ind,,]
    }

    ## Setup
    ng <- dim(Vector)[1]
    nvr <- dim(Vector)[3]
    jData <- list(v=Vector, ng=ng, nvr=nvr, vtr=vtr)
    monitor <- c('sigma', 'alpha', 'beta', 'omega', 'a', 'b')
    nsig <- 2
    FUN <- function(chain) {list(log.sigma=runif(nsig))}
    inits <- mapply(FUN, 1:4, SIMPLIFY=FALSE)

    require(runjags)
    runjags.options(rng.warning=FALSE)
    jagsfit <- run.jags(model, monitor=monitor, data=jData, inits=inits, n.chains=length(inits),
                        method="rjparallel", adapt = 1500, burnin=1500, sample=1500, summarise=FALSE)
    save(jagsfit, Vector, thresh, file=outfile)

                                        # Inspection
    if (debug & !exists('script')) {
        fit <- convert(jagsfit)
        fit
        plot(fit)
        R2WinBUGS:::attach.bugs(fit)

                                        # Omega
        Hist(omega[,1], n=100)
        Hist(omega[,2], n=100)

        plot(apply(alpha, 2, mean), apply(beta, 2, mean), pch=19)

        ma <- apply(alpha, 2, mean)
        va <- log(apply(Vector[,1,], 1, mean))
        plot(va, ma, pch=19)
    }
}
