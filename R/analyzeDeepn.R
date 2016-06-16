analyzeDeepn <- function(infile, outfile="out.txt", debug=FALSE, sort=1) {
  Data <- importFromDeepn(infile)
  Data$omega <- overdisp(Data)
  if (debug) Data <- applyFilter(Data, thresh=200)
  runMCMC(Data)
  object <- psm()
  summary(object, sort=sort, outfile=outfile)
}
