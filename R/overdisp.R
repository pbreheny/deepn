overdisp <- function(Data) {
  omega <- c(estimateCommonDisp(DGEList(Data$Vector[,1,]))$common.dispersion,
             estimateCommonDisp(DGEList(Data$Vector[,2,]))$common.dispersion)
  names(omega) <- dimnames(Data$Vector)[[2]]
  omega
}
