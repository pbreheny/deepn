analyzeDeepn <- function(infile, outfile="stat.csv", debug=FALSE, sort=1) {
  Data <- importFromDeepn(infile)
  Data$omega <- overdisp(Data)
  if (debug) Data <- applyFilter(Data, thresh=200)
  fit <- runMCMC(Data)
  object <- psm(fit)
  summary(object, sort=sort, outfile=outfile)
}
