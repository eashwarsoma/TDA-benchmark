library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

unifcircle <- function(circle.points, circle.dimensions) {
  if (circle.dimensions == 2) {
    list.parameters <- data.frame() 
    list.parameters <- runif(circle.points, 0, 2*pi)
    
    x1 <- list.parameters
    list.unifcircle <- data.frame()
    list.unifcircle <- cbind(cos(x1), sin(x1))
  }
  
  if (circle.dimensions == 3) {
    list.parameters <- data.frame() 
    repeat {
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      if (x^2 + y^2 < 1) {
        list.parameters[nrow(list.parameters)+1,1:2 ] <- rbind(x,y)
      }
      if (nrow(list.parameters) == circle.points) {
        break
      }
    }
    x1 <- select(list.parameters, 1)
    x2 <- select(list.parameters, 2)
    list.unifcircle <- data.frame()
    list.unifcircle <- cbind(2*x1*sqrt(1-x1^2-x2^2), 2*x2*sqrt(1-x1^2-x2^2), 1-2*(x1^2+x2^2))
  }
  
  if (circle.dimensions == 4) {
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
  to.calc.hom <- data.matrix(list.unifcircle, rownames.force = NA)
  return(to.calc.hom)
}