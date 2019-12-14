#####Function to make enclosing radius for TDA GUDHI and Dionysus (makes runtime fairer)#####
enclosing_radius <- function(X){
  # function which finds radius beyond which no homology changes
  # X is a point cloud data frame
  d = dist(X)
  n = nrow(X)
  return(min(unlist(lapply(X = 1:(nrow(X) - 1), FUN = function(X){ return(max(d[(1+(X-1)*n-(X-1)*X/2):(X*n-X*(X+1)/2)])) }))))
}


