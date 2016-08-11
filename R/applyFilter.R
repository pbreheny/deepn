# Need to make this work for when Data is just vector
applyFilter <- function(Data, thresh) {
  RPM <- rpm(Data)

  # Nonsel filter
  if (Data$multiBait) {
    N <- cbind(apply(RPM$Vector[,1,], 1, mean), RPM$Bait[,1,])
  } else {
    N <- cbind(apply(RPM$Vector[,1,], 1, mean), RPM$Bait[,1])
  }
  bPass <- apply(N > thresh, 1, all)

  # Sel filter
  if (Data$multiBait) {
    S <- cbind(RPM$Vector[,2,], RPM$Bait[,2,])
  } else {
    S <- cbind(RPM$Vector[,2,], RPM$Bait[,2])
  }
  sPass <- apply(S > thresh, 1, any)

  # Subset and return
  ind <- which(bPass & sPass)
  Data$Vector <- Data$Vector[ind,,]
  Data$Bait <- if (Data$multiBait) Data$Bait[ind,,] else Data$Bait[ind,]
  Data
}
rpm <- function(Data) {
  Vector <- sweep(Data$Vector, 2:3, Data$vtr, "/")*1e6
  if (!is.null(Data$Bait)) {
    if (Data$multiBait) {
      Bait <- sweep(Data$Bait, 2:3, Data$btr, "/")*1e6
    } else {
      Bait <- sweep(Data$Bait, 2, Data$btr, "/")*1e6
    }
  } else{
    Bait <- NULL
  }
  list(Vector=Vector, Bait=Bait)
}
