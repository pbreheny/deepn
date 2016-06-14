importFromDeepn <- function(file) {
  raw <- readLines(file)
  config <- matrix(trimws(unlist(strsplit(raw, "="))), ncol=2)
}
