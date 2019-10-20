# NB: maxscale = 5 is used in TDA package examples, so used here (no default)

# point data input is required. program that is calculated is based off text string
# dimensional features and iteration number for benchmark should also be specified
bench <- function(pointdata, whichTDA, featdim, iter) {
  # TDAstats
  if (whichTDA == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim),
                 iterations = iter)
  # TDA - Dionysus
  } else if (whichTDA == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5,
                          location = FALSE, library = "Dionysus"),
                 iterations = iter)
  # TDA - GUDHI
  } else if (whichTDA == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5,
                          location = FALSE, library = "GUDHI"),
                 iterations = iter)
  # TDA - GUDHI (alpha complex)
  } else if (whichTDA == "GUDHIalpha") {
    time <- mark(alphaComplexDiag(pointdata, maxdimension = featdim,
                                  location = FALSE, library = "GUDHI"),
                 iterations = iter)
  # none of the above
  } else {
    stop("Invalid TDA engine selected")
  }
  
  # row 1 column 3 is specifically median bench time
  return(time[1,3])
}
