analyzeDeepn <- function(infile, outfile="stat.csv", debug=FALSE, sort=1, port) {
  Data <- importFromDeepn(infile)
  Data$omega <- overdisp(Data)
  browser()
  if (!missing(port)) {
    con <- socketConnection(host="localhost", port=port)
    out <- c("Overdispersion estimates",
             paste("Baseline: ", formatC(Data$omega["Baseline"], digits=2, format="f")),
             paste("Selection:", formatC(Data$omega["Selected"], digits=2, format="f")),
             "-----",
             "Baseline overdispersion should be around 0 (no overdispersion)",
             "Overdispersion under selection conditions will ideally be around 1 or lower")
    writeLines(out, con)
    close(con)
  }
  if (debug) Data <- applyFilter(Data, thresh=200)
  fit <- runMCMC(Data)
  object <- psm(fit)
  summary(object, sort=sort, outfile=outfile)
}
