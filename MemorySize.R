library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

memory <- function(pointdata, whichTDA, featdim, iter) { #point data input is required. program that is calculated is based off text string. Dimensional features and iteration number for benchmark should also be specified
  if (whichTDA == "stats") { 
    phom <- calculate_homology(pointdata, dim = featdim)
    size <- object_size(phom)
    return(size)
  }
  if (whichTDA == "Dionysus") { 
    phom <- ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "Dionysus")
    size <- object_size(phom)
    return(size)
  }
  if (whichTDA == "GUDHI") { 
    phom <- ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "GUDHI")
    size <- object_size(phom)
    return(size)
  }
  if (whichTDA == "GUDHIalpha") { 
    phom <- alphaComplexDiag(pointdata, maxdimension = featdim, location = FALSE, library = "GUDHI")
    size <- object_size(phom)
    return(size)
  }
}