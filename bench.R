bench <- function(pointdata, whichTDA, featdim, iter) { #point data input is required. program that is calculated is based off text string. Dimensional features and iteration number for benchmark should also be specified
  if (whichTDA == "stats") { 
    time <- mark(calculate_homology(pointdata, dim = featdim), iterations = iter)
  }
  if (whichTDA == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "Dionysus"), iterations = iter)
  }
  if (whichTDA == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "GUDHI"), iterations = iter)
  }
  if (whichTDA == "GUDHIalpha") {
    time <- mark(alphaComplexDiag(pointdata, maxdimension = featdim, location = FALSE, library = "GUDHI"), iterations = iter)
  }
  return(time[1,3]) #row 1 column 3 is specifically median bench time
}
