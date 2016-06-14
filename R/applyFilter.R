applyFilter <- function(Data, thresh) {

  # Base filter
  v <- apply(Data$Vector[,1,], 1, mean)
  if ("Bait" %in% names(Data)) {
    arrayBait <- length(dim(Data$Bait)) > 2
    if (arrayBait) {
      B <- cbind(v, Data$Bait[,1,])
    } else {
      B <- cbind(v, Data$Bait[,1])
    }
  } else {
    B <- matrix(v, ncol=1)
  }
  BPPM <- sweep(B, 2, apply(B, 2, sum), '/')*1e6
  bPass <- apply(BPPM > thresh, 1, all)

  # Sel filter
  if ("Bait" %in% names(Data)) {
    v <- apply(Data$Vector[,2,], 1, mean)
    if (arrayBait) {
      S <- cbind(v, Data$Bait[,2,])
    } else {
      S <- cbind(v, Data$Bait[,2])
    }
    SPPM <- sweep(S, 2, apply(S, 2, sum), '/')*1e6
    sPass <- apply(SPPM > thresh, 1, any)
  } else {
    sPass <- rep(TRUE, length(bPass))
  }

  # Subset and return
  ind <- which(bPass & sPass)
  Data$Vector <- Data$Vector[ind,,]
  if ("Bait" %in% names(Data)) {
    if (arrayBait) {
      Data$Bait <- Data$Bait[ind,,]
    } else {
      Data$Bait <- Data$Bait[ind,]
    }
  }
  Data
}
