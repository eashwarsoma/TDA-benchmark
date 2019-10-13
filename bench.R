library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

bench <- function(pointdata, whichTDA, featdim, iter) {
  if (whichTDA == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim), iterations = iter)
  }
  if (whichTDA == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "Dionysus"), iterations = iter)
  }
  if (whichTDA == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "GUDHI"), iterations = iter)
  }
  return(time[1,3])
}
