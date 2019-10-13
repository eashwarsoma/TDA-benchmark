library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

#almost idential to unif circle, but all x and y coordinates are multiplied by a perturbance varying from .9 to 1.1
noisycircle <- function(noisycircle.points, noisycircle.dimensions) {
  if (noisycircle.dimensions == 2) {
    list.parameters <- data.frame() 
    list.parameters <- runif(noisycircle.points, 0, 2*pi)
    
    x1 <- list.parameters
    list.noisycircle <- data.frame()
    x_noise <- runif(noisycircle.points, .9, 1.1) #generates x noise
    y_noise <- runif(noisycircle.points, .9, 1.1) #generates y noise
    list.noisycircle <- cbind(x_noise*cos(x1), y_noise*sin(x1)) #each coordinate is multiplied by respective noise
  }
  
  if (noisycircle.dimensions == 3) {
    list.parameters <- data.frame() 
    repeat { #idential to unifcircle
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      if (x^2 + y^2 < 1) {
        list.parameters[nrow(list.parameters)+1,1:2 ] <- rbind(x,y)
      }
      if (nrow(list.parameters) == noisycircle.points) {
        break
      }
    }
    x1 <- select(list.parameters, 1)
    x2 <- select(list.parameters, 2)
    list.noisycircle <- data.frame()
    x_noise <- runif(noisycircle.points, .9, 1.1) #now we need three perturbance vectors, one for each coordinate, multiplied the same way as before
    y_noise <- runif(noisycircle.points, .9, 1.1)
    z_noise <- runif(noisycircle.points, .9, 1.1)
    list.noisycircle <- cbind(x_noise*2*x1*sqrt(1-x1^2-x2^2), y_noise*2*x2*sqrt(1-x1^2-x2^2),z_noise*(1-2*(x1^2+x2^2)))
  }
  
  if (noisycircle.dimensions == 4) {
    list.parameters <- data.frame() 
    repeat {
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      z <- runif(1, -1, 1)
      w <- runif(1, -1, 1)
      if (x^2 + y^2 < 1 & z^2 + w^2 < 1 ) {
        list.parameters[nrow(list.parameters)+1,1:4] <- rbind(x,y,z,w)
      }
      if (nrow(list.parameters) == noisycircle.points) {
        break
      }
    }
    x1 <- select(list.parameters, 1)
    x2 <- select(list.parameters, 2)
    x3 <- select(list.parameters, 3)
    x4 <- select(list.parameters, 4)
    list.noisycircle <- data.frame()
    x_noise <- runif(noisycircle.points, .9, 1.1) #same thing but with 4 noise multipliers
    y_noise <- runif(noisycircle.points, .9, 1.1)
    z_noise <- runif(noisycircle.points, .9, 1.1)
    w_noise <- runif(noisycircle.points, .9, 1.1)
    list.noisycircle <- cbind(x_noise*x1, y_noise*x2, z_noise*x3*sqrt((1-x1^2-x2^2)/(x3^2+x4^2)), w_noise*x4*sqrt((1-x1^2-x2^2)/(x3^2+x4^2)))
  }
  to.calc.hom <- data.matrix(list.noisycircle, rownames.force = NA)
  return(to.calc.hom)
}
