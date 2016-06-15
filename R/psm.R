psm <- function(mcfile="mcmc.RData", outfile="psm.RData") {
  load(mcfile)
  X <- as.matrix(jagsfit$mcmc)
  pmx <- apply(X, 2, median)
  psx <- apply(X, 2, mad)
  v <- varnames(jagsfit$mcmc)
  i <- as.numeric(gsub(",.+", "", gsub(".+\\[", "", v)))
  j <- as.numeric(gsub("\\]", "", gsub(".+,", "", v)))
  pm <- ps <- matrix(NA, max(i), max(j))
  for (m in 1:ncol(X)) {
    pm[i[m],j[m]] <- pmx[m]
    ps[i[m],j[m]] <- psx[m]
  }
  dimnames(pm) <- dimnames(ps) <- dimnames(Data$Bait)[c(1,3)]
  save(Data, pm, ps, file=outfile)
}
