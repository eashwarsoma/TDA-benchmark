library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

source("generate-data.R")
source("bench.R")
source("MemorySize.R") # need to review

###FIX THIS LATER ON###
#need if-then statements to stop function if (for example)
#  feature.dim > data.dimension
#make more user-friendly
TDA_bench <- function(measure, data.type, data.dimensions, num.points,
                      feature.dimensions, TDA.library, num.iteration) {
  #step 1, generate the dataset
  if (data.type == "circle") {
    pointdata <- unifcircle(num.points, data.dimensions)
  } else if (data.type == "uniform") {
    pointdata <- unifbox(num.points, data.dimensions)
  } else if (data.type == "annulus") {
    pointdata <- noisycircle(num.points, data.dimensions)
  } else if (data.type == "torus") {
    pointdata <- torus(num.points)
  } else {
    stop("Invalid data type")
  }
  
  # step 2 benchmark
  if (measure == "time") {
    exec.time <- bench(pointdata, TDA.library,
                       feature.dimensions, num.iteration)
    exec.time <- exec.time[[1,1]]
    return(exec.time)
  }
  if (measure == "memory") {
    mem.data <- memory(pointdata, TDA.library,
                       feature.dimensions)
    return(mem.data)
  }
}

test1 <- TDA_bench(measure = "time", data.type = "uniform",
                   data.dimensions = 3, num.points = 150,
                   feature.dimensions = 1, TDA.library = "GUDHIalpha",
                   num.iteration = 11)

test2 <- TDA_bench(measure = "time", data.type = "uniform",
                   data.dimensions = 3, num.points = 150,
                   feature.dimensions = 1, TDA.library = "stats",
                   num.iteration = 11)
