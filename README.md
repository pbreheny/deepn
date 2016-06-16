# Dynamic Enrichment for Evaluation of Protein Networks

This is the accompanying `R` package for the [DEEPN](https://github.com/emptyewer/DEEPN) project.

## Installation

`deepn` has several package dependencies, most notably [JAGS](http://mcmc-jags.sourceforge.net) and [Bioconductor](https://www.bioconductor.org).  JAGS must be setup outside `R` (see link for instructions), while Bioconductor can be installed within `R` via:

```r
source("https://bioconductor.org/biocLite.R")
biocLite()
```

Once that is done, you can install `deepn` via `devtools` (which itself can be installed via `install.packages("devtools")`):

```r
devtools::install_github("pbreheny/deepn")
```

## Basic usage

Once installed, `deepn` can be used to analyze the output from `deepn` with

```r
require(deepn)
Data <- importFromDeepn("deepn.config")
Data$omega <- overdisp(Data)
runMCMC(Data) 
```

where `"deepn.config"` is the name of the configuration file that DEEPN outputs containing path names to all the `.csv` files needed in the analysis.
