#Uses the sphere picking tactic to make uniform distribution 
unifcircle <- function(circle.points, circle.dimensions) {
  # var that stores result (empty df setup)
  to.calc.hom <- matrix(NA, nrow = circle.points, ncol = circle.dimensions)
  to.calc.hom <- as.data.frame(to.calc.hom)
  
  # returns 2-d circle data
  if (circle.dimensions == 2) {
    angles <- runif(circle.points, 0, 2*pi)
    to.calc.hom <- cbind(cos(angles), sin(angles))
  }
  
  # returns 3-d circle data
  if (circle.dimensions == 3) {
    
    # each loop generates one row of data
    for (curr_row in 1:circle.points) {
      
      # generate valid x1 and x2
      x1 <- runif(1, -1, 1)
      x2 <- runif(1, -1, 1)
      while (x1 ^ 2 + x2 ^ 2 >= 1) {
        x1 <- runif(1, -1, 1)
        x2 <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      x <- 2 * x1 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      y <- 2 * x2 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      z <- 1 - 2 * (x1 ^ 2 + x2 ^ 2)
      
      # store into data frame
      to.calc.hom[curr_row, ] <- rbind(x, y, z)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  if (circle.dimensions == 4) { #follows same principle as previous but with more parameters
    # each loop generates one row of data
    for (curr_row in 1:circle.points) {
      
      # generate valid w, x, y, z
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      z <- runif(1, -1, 1)
      w <- runif(1, -1, 1)
      while (x ^ 2 + y ^ 2 >= 1 |
             w ^ 2 + z ^ 2 >= 1) {
        w <- runif(1, -1, 1)
        x <- runif(1, -1, 1)
        y <- runif(1, -1, 1)
        z <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      temp <- sqrt((1 - x ^ 2 - y ^ 2) / (w ^ 2 + z ^ 2))
      x1 <- x
      x2 <- y
      x3 <- z * temp
      x4 <- w * temp
      
      # store into data frame
      to.calc.hom[curr_row, ] <- rbind(x1, x2, x3, x4)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  # return answer variable
  return(to.calc.hom)
}
