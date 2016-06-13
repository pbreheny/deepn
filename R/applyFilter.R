applyFilter <- function(thresh=0, Vector, Data) {
  # Base filter
  v <- apply(Vector[,1,], 1, mean)
  if (!missing(Data)) {
    arrayData <- length(dim(Data)) > 2
    if (arrayData) {
      B <- cbind(v, Data[,1,])
    } else {
      B <- cbind(v, Data[,1])
    }
  } else {
    B <- matrix(v, ncol=1)
  }
  BPPM <- sweep(B, 2, apply(B, 2, sum), '/')*1e6
  bPass <- apply(BPPM > thresh, 1, all)

  # Sel filter
  if (!missing(Data)) {
    v <- apply(Vector[,2,], 1, mean)
    if (arrayData) {
      S <- cbind(v, Data[,2,])
    } else {
      S <- cbind(v, Data[,2])
    }
    SPPM <- sweep(S, 2, apply(S, 2, sum), '/')*1e6
    sPass <- apply(SPPM > thresh, 1, any)
  } else {
    sPass <- rep(TRUE, length(bPass))
  }

  # Subset and return
  ind <- which(bPass & sPass)
  val <- list(Vector=Vector)
  if (!missing(Data)) {
    if (arrayData) {
      val$Data <- Data[ind,,]
    } else {
      val$Data <- Data[ind,]
    }
  }
  val
}
