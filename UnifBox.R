# generates an n-dimension box (0, 1) of uniformly distributed points
unifbox <- function(box.points, box.dimensions) {
  # empty 
  to.calc.hom <- matrix(NA, nrow = box.points, ncol = box.dimensions)
  
  # adds a column of randomaly generated points 0 to 1 for how many ever dimensions specified
  for (i in 1:box.dimensions) {
    col <- runif(box.points, 0, 1)
    to.calc.hom[, i] <- col
  }
  
  return(to.calc.hom)
}