# NB: maxscale = 5 is used in TDA package examples, so used here (no default)

# point data input is required. program that is calculated is based off text string
# dimensional features and iteration number for benchmark should also be specified
bench <- function(pointdata, TDA.library, featdim, num.iterations) {
  # TDAstats
  if (TDA.library == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim),
                 iterations = num.iterations)
    # TDA - Dionysus
  } else if (TDA.library == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5,
                          location = FALSE, library = "Dionysus"),
                 iterations = num.iterations)
    # TDA - GUDHI
  } else if (TDA.library == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 5,
                          location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # TDA - GUDHI (alpha complex)
  } else if (TDA.library == "GUDHIalpha") {
    time <- mark(alphaComplexDiag(pointdata, maxdimension = featdim,
                                  location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # none of the above
  } else {
    stop("Invalid TDA engine selected")
  }
  
  # row 1 column 12 is list of times
  return(time[1,12])
}
