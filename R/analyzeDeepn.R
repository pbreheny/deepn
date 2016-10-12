analyzeDeepn <- function(infile, outfile="stat.csv", debug=FALSE, sort=1, port) {
  Data <- importFromDeepn(infile)
  Data$omega <- overdisp(Data)
  if (!missing(port)) {
    con <- socketConnection(host="localhost", port=port)
    out <- c("Overdispersion estimates",
             paste("Baseline (vector only):   ", formatC(Data$omega["Baseline"], digits=2, format="f")),
             paste("Baseline (vector + bait): ", formatC(Data$omega["baitEffect"], digits=2, format="f")),
             paste("Selection:                ", formatC(Data$omega["Selected"], digits=2, format="f")),
             "-----",
             "Baseline overdispersions should be around 0 (no overdispersion)",
             "Overdispersion under selection conditions will ideally be around 1 or lower")
    if (Data$omega["baitEffect"] > 2*Data$omega["Baseline"] & Data$omega["baitEffect"] > .15) {
      out <- c(out, "", "WARNING: There appears to be evidence of secondary bait effects.",
               "Abundances in presence of bait are different from abundances in absence of",
               "bait, even under non-selective conditions.  This generally indicates that",
               "the bait is disrupting growth or sequencing in unintended ways.")
    }
    writeLines(out, con)
    close(con)
  }
  if (debug) Data <- applyFilter(Data, thresh=200)
  fit <- runMCMC(Data)
  object <- psm(fit)
  summary(object, sort=sort, outfile=outfile)
}
