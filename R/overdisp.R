disp.edgeR <- function(Y, ind=1:ncol(Y)) {
  dge <- DGEList(counts=Y[,ind], group=Group[ind])
  estimateCommonDisp(dge)$common.dispersion
}
