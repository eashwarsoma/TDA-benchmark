#Uses the sphere picking tactic to make uniform distribution 
unifcircle <- function(circle.points, circle.dimensions) {
  # var that stores result
  to.calc.hom <- NULL
  
  # returns 2-d circle data
  if (circle.dimensions == 2) {
    angles <- runif(circle.points, 0, 2*pi)
    to.calc.hom <- cbind(cos(angles), sin(angles))
  }
  
  if (circle.dimensions == 3) {
    list.parameters <- data.frame() 
    repeat { #this loop generates the necessary parameters to make a uniform sphere
      x <- runif(1, -1, 1) #pick one point at a time
      y <- runif(1, -1, 1)
      if (x^2 + y^2 < 1) { #if this condition is satisfied, add it to the parameter list at the next row
        list.parameters[nrow(list.parameters)+1,1:2 ] <- rbind(x,y)
      }
      if (nrow(list.parameters) == circle.points) {
        break #this loop will repeat until the number of parameters equals the number of circular points
      }
    }
    x1 <- select(list.parameters, 1) #isolates the first parameter
    x2 <- select(list.parameters, 2) #isolates the second parameter
    list.unifcircle <- data.frame()
    list.unifcircle <- cbind(2*x1*sqrt(1-x1^2-x2^2), 2*x2*sqrt(1-x1^2-x2^2), 1-2*(x1^2+x2^2)) #math equation to generate the sphere
  }
  
  if (circle.dimensions == 4) { #follows same principle as previous but with more parameters
    list.parameters <- data.frame() 
    repeat {
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      z <- runif(1, -1, 1)
      w <- runif(1, -1, 1)
      if (x^2 + y^2 < 1 & z^2 + w^2 < 1 ) {
        list.parameters[nrow(list.parameters)+1,1:4] <- rbind(x,y,z,w)
      }
      if (nrow(list.parameters) == circle.points) {
        break
      }
    }
    x1 <- select(list.parameters, 1)
    x2 <- select(list.parameters, 2)
    x3 <- select(list.parameters, 3)
    x4 <- select(list.parameters, 4)
    list.unifcircle <- data.frame()
    list.unifcircle <- cbind(x1, x2, x3*sqrt((1-x1^2-x2^2)/(x3^2+x4^2)), x4*sqrt((1-x1^2-x2^2)/(x3^2+x4^2)))
  }
  #to.calc.hom <- data.matrix(list.unifcircle, rownames.force = NA) #data is converted from data frame to matrix
  return(to.calc.hom) #every function will create this matrix
}
