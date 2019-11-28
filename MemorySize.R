#point data input is required. program that is calculated is based off 
#text string. Dimensional features and iteration number for benchmark 
#should also be specified. Uses rips filtration function
memory <- function(pointdata, TDA.library, feature.dimensions) { 
  if (TDA.library == "Dionysus") { 
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                           library = "Dionysus")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHI") {
    print("1")
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                           library = "GUDHI")
    print("2")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHIalpha") {
    print("1")
    filtrate <- alphaComplexFiltration(pointdata, 
                                   library = "GUDHI")
    print("2")
    size <- object_size(filtrate[[1]])
    
  } else {
    stop("Choose 'Dionysus', 'GUDHI', or 'GUDHIalpa'. Cannot use TDAstats")
  }
  
  return(size)
}

#change function to filtration function and calculate memory size 
#TDA section only location = TRUE or FALSE and memory only
#mclcoreapply package parralel
