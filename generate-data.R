# add if statements to check for parameter validity

#####BOX DATA#####
# generates an n-dimension box (0, 1) of uniformly distributed points
unifbox <- function(num.points, data.dimensions) {
  # empty 
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  
  # adds a column of randomaly generated points 0 to 1 for how many ever dimensions specified
  for (i in 1:data.dimensions) {
    col <- runif(num.points, 0, 1)
    to.calc.hom[, i] <- col
  }
  
  return(to.calc.hom)
}

#####TORUS DATA#####
# very simple, uses native uniform torus function from TDA
torus <- function(num.points) {
  torusUnif(num.points, 1, 1)
}

#####UNIFORM CIRCLE DATA#####
# Uses the sphere picking tactic to make uniform distribution 
# Cite Marsaglia paper from Wolfram Alpha page
unifcircle <- function(num.points, data.dimensions) {
  # var that stores result (empty df setup)
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  #to.calc.hom <- as.data.frame(to.calc.hom)
  
  # returns 2-d circle data
  if (data.dimensions == 2) {
    angles <- runif(num.points, 0, 2*pi)
    to.calc.hom <- cbind(cos(angles), sin(angles))
  }
  
  # returns 3-d circle data
  if (data.dimensions == 3) {
    
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
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
      to.calc.hom[curr_row, ] <- c(x, y, z)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  if (data.dimensions == 4) { #follows same principle as previous but with more parameters
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
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
      to.calc.hom[curr_row, ] <- c(x1, x2, x3, x4)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  # return answer variable
  return(to.calc.hom)
}

#####NOISY CIRCLE DATA#####
# almost idential to unif circle, but all x and y coordinates are multiplied by a perturbance varying from .9 to 1.1
noisycircle <- function(num.points, data.dimensions,
                        noise.magnitude = 0.1) {
  # get non-noisy data
  to.calc.hom <- unifcircle(num.points, data.dimensions)
  
  # add noise
  for (curr.col in 1:data.dimensions) {
    noise <- runif(num.points, 1 - noise.magnitude,
                   1 + noise.magnitude)
    to.calc.hom[, curr.col] <- to.calc.hom[, curr.col] * noise
  }
  
  # return noisy data
  return(to.calc.hom)
}
