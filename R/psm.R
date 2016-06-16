psm <- function(mcfile="mcmc.RData") {
  load(mcfile)
  X <- as.matrix(jagsfit$mcmc)
  if (Data$arrayBait) {
    dim(X) <- c(nrow(X), dim(Data$Bait)[1], dim(Data$Bait)[3])
    pm <- cbind(apply(X, 2:3, median), apply(X[,,2]-X[,,1], 2, median))
    ps <- cbind(apply(X, 2:3, mad), apply(X[,,2]-X[,,1], 2, mad))
    b <- dimnames(Data$Bait)[[3]]
    colnames(pm) <- colnames(ps) <- c(b, paste(b[2], b[1], sep="_"))
    rownames(pm) <- rownames(ps) <- dimnames(Data$Bait)[[1]]
  } else {
    pm <- apply(X, 2, median)
    ps <- apply(X, 2, mad)
    names(pm) <- names(ps) <- dimnames(Data$Bait)[[1]]
  }
  structure(list(Data=Data, pm=pm, ps=ps),
            class = "psm.deepn")
}
