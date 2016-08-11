overdisp <- function(Data) {
  V <- Data$Vector
  omega <- c(edgeR::estimateCommonDisp(edgeR::DGEList(V[,1,]))$common.dispersion,
             edgeR::estimateCommonDisp(edgeR::DGEList(V[,2,]))$common.dispersion)
  names(omega) <- dimnames(V)[[2]]
  omega
}
