overdisp <- function(Data) {
  omega <- c(edgeR::estimateCommonDisp(edgeR::DGEList(Data$Vector[,1,]))$common.dispersion,
             edgeR::estimateCommonDisp(edgeR::DGEList(Data$Vector[,2,]))$common.dispersion)
  names(omega) <- dimnames(Data$Vector)[[2]]
  omega
}
