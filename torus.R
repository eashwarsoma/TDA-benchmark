library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

torus <- function(torus.points) { #very simple, uses native uniform torus function from TDA
  to.calc.hom <- torusUnif(torus.points, 1, 1)
  return(to.calc.hom)
}