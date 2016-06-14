importFromDeepn <- function(file) {
  raw <- readLines(file)
  config <- matrix(trimws(unlist(strsplit(raw, "="))), ncol=2, byrow=TRUE)
  key <- config[,1]
  val <- config[,2]
  vn <- val[grep("Vector_Non", key)]
  vs <- val[grep("Vector_Sel", key)]
  bn <- val[grep("Bait._Non", key)]
  bs <- val[grep("Bait._Sel", key)]
  Data <- list(Vector = readDeepn(vn, vs),
               Bait = readDeepn(bn, bs),
               Threshold = val[key=="Threshold"])
  if (!all.equal(dimnames(Data$Vector)[[1]], dimnames(Data$Bait)[[1]])) stop("Genes don't match between vector and bait")
  Data
}
