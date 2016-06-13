subsetData <- function(Data) {
  x1 <- apply(Data[,1,]!=0, 1, all)
  x2 <- apply(Data[,2,]!=0, 1, any)
  s <- which(x1 & x2)
  Data[s,,]
}
