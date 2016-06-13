bpsm <- function(mcfile="mcmc.RData", outfile="psm.RData") {
    load(mcfile)
    require(runjags)
    fit <- convert(jagsfit)
    R2WinBUGS:::attach.bugs(fit)
    pm <- ps <- NULL
    arrayData <- length(dim(Data)) > 2

    # Posterior summary
    if (arrayData) {
        require(abind)
        delta <- abind(gamma, gamma[,,2]-gamma[,,1], along=3)
        pm$delta <- apply(delta, 2:3, median)
        ps$delta <- apply(delta, 2:3, mad)
        rownames(pm$delta) <- rownames(ps$delta) <- dimnames(Data)[[1]]
        Bait <- dimnames(Data)[[3]]
        colnames(pm$delta) <- colnames(ps$delta) <- c(Bait[1], Bait[2], paste0(Bait[2], "_", Bait[1]))
    } else {
        pm$gamma <- apply(gamma, 2, median)
        ps$gamma <- apply(gamma, 2, mad)
        names(pm$gamma) <- names(ps$gamma) <- rownames(Data)
    }

    # Export
    save(pm, ps, file=outfile)
}
