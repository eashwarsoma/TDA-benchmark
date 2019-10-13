library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

torus <- function(torus.points) {
  to.calc.hom <- torusUnif(torus.points, 1, 1)
  return(to.calc.hom)
}