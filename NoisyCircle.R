# get unifcircle function
source("UnifCircle.R")

# almost idential to unif circle, but all x and y coordinates are multiplied by a perturbance varying from .9 to 1.1
noisycircle <- function(circle.points, circle.dimensions,
                        noise.magnitude = 0.1) {
  # get non-noisy data
  to.calc.hom <- unifcircle(circle.points, circle.dimensions)
  
  # add noise
  for (curr.col in 1:circle.dimensions) {
    noise <- runif(circle.points, 1 - noise.magnitude,
                                  1 + noise.magnitude)
    to.calc.hom[, curr.col] <- to.calc.hom[, curr.col] * noise
  }
  
  # return noisy data
  return(to.calc.hom)
}
