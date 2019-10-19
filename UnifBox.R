unifbox <- function(box.points, box.dimensions) {
  list <- data.frame() 
  for (i in 1:box.dimensions) { #adds a column of randomaly generated points 0 to 1 for how many ever dimensions specified
    col <- runif (box.points, 0, 1) #number of points is based off function input
    list[1:box.points, i] <- cbind(col)
  }
  to.calc.hom <- data.matrix(list, rownames.force = NA)
  return(to.calc.hom)
}