import <- function(vn, vs, bn, bs) {
  Vector <- readDeepn(vn, vs)
  Bait <- readDeepn(bn, bs)
  arrayBait <- length(dim(Bait))==3
  if (!all.equal(dimnames(Vector)[[1]], dimnames(Bait)[[1]])) stop("Genes don't match between vector and bait")
  list(Vector = Vector,
       Bait = Bait,
       arrayBait = arrayBait,
       Gene = dimnames(Vector)[[1]],
       vtr <- apply(Vector, 2:3, sum),
       btr <- ifelse(arrayBait, apply(Bait, 2:3, sum), apply(Bait, 2, sum))
  )
}
