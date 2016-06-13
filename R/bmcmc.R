bmcmc <- function(vdat, bdat, vpsm, outfile, n.chains=4, debug=TRUE) {
  # Load
  load(vdat)
  load(bdat)
  load(vpsm)

  # Filter low/zero counts
  res <- applyFilter(thresh, Vector, Data)
  Vector <- res$Vector
  Data <- res$Data
  arrayData <- length(dim(Data)) > 2
  if (arrayData) {
    ytr <- apply(Data, 2:3, sum)
  } else {
    ytr <- apply(Data, 2, sum)
  }
  mtch <- match(rownames(Data), names(pm$alpha))

  # For debugging purposes
  if (debug) {
    ind <- sample(1:dim(Data)[1], 20)
    if (arrayData) {
      Data <- Data[ind,,]
    } else {
      Data <- Data[ind,]
    }
  }

  # Setup
  ng <- dim(Data)[1]
  monitor <- c('gamma')
  if (arrayData) {
    nb <- dim(Data)[3]
    jData <- list(y=Data, ng=ng, nb=nb, ytr=ytr, omega=pm$omega, ma=pm$alpha[mtch], sa=ps$alpha[mtch], mb=pm$beta[mtch], sb=ps$beta[mtch])
    model <- system.file("model/sModel2.jag", package="deepn")
  } else {
    jData <- list(y=Data, ng=ng, ytr=ytr, omega=pm$omega, ma=pm$alpha[mtch], sa=ps$alpha[mtch], mb=pm$beta[mtch], sb=ps$beta[mtch])
    model <- system.file("model/sModel1.jag", package="deepn")
  }
  nsig <- 1
  FUN <- function(chain) {list(log.sigma=runif(nsig))}
  inits <- mapply(FUN, 1:n.chains, SIMPLIFY=FALSE)

  runjags.options(rng.warning=FALSE)
  jagsfit <- run.jags(model, monitor=monitor, data=jData, inits=inits, n.chains=n.chains,
                      method="rjparallel", adapt = 1500, burnin=1500, sample=1500, summarise=FALSE)
  save(jagsfit, Data, file=outfile)
}
