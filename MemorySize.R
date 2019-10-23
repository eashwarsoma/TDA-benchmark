#point data input is required. program that is calculated is based off 
#text string. Dimensional features and iteration number for benchmark 
#should also be specified. Uses rips filtration function
memory <- function(pointdata, whichTDA, featdim, iter) { 
  if (whichTDA == "Dionysus") { 
    phom <- ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "Dionysus")
    size <- object_size(phom)
    
  } else if (whichTDA == "GUDHI") { 
    phom <- ripsDiag(pointdata, maxdimension = featdim, maxscale = 5, location = FALSE, library = "GUDHI")
    size <- object_size(phom)
    
  } else if (whichTDA == "GUDHIalpha") { 
    phom <- alphaComplexDiag(pointdata, maxdimension = featdim, location = FALSE, library = "GUDHI")
    size <- object_size(phom)
    
  } else {
    stop("Choose 'Dionysus', 'GUDHI', or 'GUDHIalpa'. Cannot use TDAstats")
  }
  
  return(size)
}

#change function to filtration function and calculate memory size 
#TDA section only location = TRUE or FALSE and memory only
#mclcoreapply package parralel
