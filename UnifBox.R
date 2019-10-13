library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

unifbox <- function(box.points, box.dimensions) {
  list <- data.frame() 
  for (i in 1:box.dimensions) {
    col <- runif (box.points, 0, 1)
    list[1:box.points, i] <- cbind(col)
  }
  to.calc.hom <- data.matrix(list, rownames.force = NA)
  return(to.calc.hom)
}