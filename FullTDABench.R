library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)

source("UnifCircle.R")
source("UnifBox.R")
source("NoisyCircle.R")
source("torus.R")
source("bench.R")



TDA_bench <- function(data.type, data.dimensions, num.points, feature.dimensions, TDA.library, num.iteration, ...) {
  if (data.type == "circle") {
    pointdata <- unifcircle(num.points, data.dimensions)
  }
  if (data.type == "uniform") {
    pointdata <- unifbox(num.points, data.dimensions)
  }
  if (data.type == "annulus") {
    pointdata <- noisycircle(num.points, data.dimensions)
  }
  if (data.type == "torus") {
    pointdata <- torus(num.points)
  }
  bench(pointdata, TDA.library, feature.dimensions, num.iteration)
}

test1 <- TDA_bench(data.type = "uniform", data.dimensions = 4, num.points = 100, feature.dimensions = 1, TDA.library = "stats", num.iteration = 11)
test2 <- TDA_bench(data.type = "uniform", data.dimensions = 8, num.points = 100, feature.dimensions = 1, TDA.library = "GUDHI", num.iteration = 10)
test3 <- TDA_bench(data.type = "uniform", data.dimensions = 12, num.points = 100, feature.dimensions = 2, TDA.library = "stats", num.iteration = 10)
test4 <- TDA_bench(data.type = "uniform", data.dimensions = 16, num.points = 100, feature.dimensions = 1, TDA.library = "stats", num.iteration = 7)
test5 <- TDA_bench(data.type = "circle", data.dimensions = 2, num.points = 100, feature.dimensions = 1, TDA.library = "GUDHI", num.iteration = 10)
test6 <- TDA_bench(data.type = "circle", data.dimensions = 3, num.points = 100, feature.dimensions = 1, TDA.library = "stats", num.iteration = 10)
#test7 <- TDA_bench(data.type = "annulus", data.dimensions = 3, num.points = 100, feature.dimensions = 2, TDA.library = "Dionysus", num.iteration = 9)
test8 <- TDA_bench(data.type = "annulus", data.dimensions = 4, num.points = 100, feature.dimensions = 1, TDA.library = "stats", num.iteration = 10)
#test9 <- TDA_bench(data.type = "circle", data.dimensions = 4, num.points = 100, feature.dimensions = 2, TDA.library = "Dionysus", num.iteration = 10)
test10 <- TDA_bench(data.type = "torus", data.dimensions = 2, num.points = 100, feature.dimensions = 1, TDA.library = "stats", num.iteration = 10)



