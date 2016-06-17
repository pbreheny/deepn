applyFilter <- function(Data, thresh) {
  rpm <- RPM(Data)

  # Nonsel filter
  if (Data$multiBait) {
    N <- cbind(apply(rpm$Vector[,1,], 1, mean), rpm$Bait[,1,])
  } else {
    N <- cbind(apply(rpm$Vector[,1,], 1, mean), rpm$Bait[,1])
  }
  bPass <- apply(N > thresh, 1, all)

  # Sel filter
  if (Data$multiBait) {
    S <- cbind(rpm$Vector[,2,], rpm$Bait[,2,])
  } else {
    S <- cbind(rpm$Vector[,2,], rpm$Bait[,2])
  }
  sPass <- apply(S > thresh, 1, any)

  # Subset and return
  ind <- which(bPass & sPass)
  Data$Vector <- Data$Vector[ind,,]
  Data$Bait <- if (Data$multiBait) Data$Bait[ind,,] else Data$Bait[ind,]
  Data
}
RPM <- function(Data) {
  Vector <- sweep(Data$Vector, 2:3, Data$vtr, "/")*1e6
  if (Data$multiBait) {
    Bait <- sweep(Data$Bait, 2:3, Data$btr, "/")*1e6
  } else {
    Bait <- sweep(Data$Bait, 2, Data$btr, "/")*1e6
  }
  list(Vector=Vector, Bait=Bait)
}
