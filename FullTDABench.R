library(dplyr)
library(readr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

source("generate-data.R")
source("bench.R")
source("MemorySize.R") # need to review see comment at end

###FIX THIS LATER ON###
#need if-then statements to stop function if (for example)
TDA_bench <- function(measure, data.type, data.dimensions, num.points,
                      feature.dimensions, TDA.library, num.iteration) {
  if (feature.dimensions > data.dimensions) {
    stop("Feature dimensions must be less than data dimensions")
  } else
  
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
    
  } else if (measure == "memory") {
    mem.data <- memory(pointdata, TDA.library,
                       feature.dimensions)
    return(mem.data)
  } else stop("Select either 'memory' or 'time' as measurement")
}

#If we use the same parameters, memory always returns the same
#value...Not sure if this is expected (is it supposed
#to be deterministic?)
test3 <- TDA_bench(measure = "time", data.type = "circle",
                   data.dimensions = 3, num.points = 50,
                   feature.dimensions = 2, TDA.library = "GUDHI",
                   num.iteration = 1)



#mapply practice look into multiple core mapplt
#creates a tibble of all variable combination
vars.test <- as_tibble(expand.grid(measure = "time", data.type = "circle",
               data.dimensions = 2:4, num.points = seq(50, 150, 50),
               feature.dimensions = 1, TDA.library = "GUDHI",
               num.iteration = 1))
#for some reason, including 0 causes whole program to crash
#figured it out, ripsDiag cannot take 0 as an input

#Uses mapply function to pass on variables to function and returns vector of time values
test.mapply <- mapply(TDA_bench, vars.test$measure, vars.test$data.type,
                     vars.test$data.dimensions, vars.test$num.points,
                     vars.test$feature.dimensions, vars.test$TDA.library,
                     vars.test$num.iteration)

#Attaches the time values back to the variables grid
vars.test$time <- test.mapply

##Measuring Time Circle Grid## 
vars.circle <- as_tibble(expand.grid(measure = "time", data.type = "circle",
                              data.dimensions = 2:4, num.points = seq(50, 500, 50),
                              feature.dimensions = 1:3, 
                              TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                              num.iteration = 10)) %>% subset(feature.dimensions < data.dimensions)
## 240 comcbination checks out with my math on paper

##Measuring Time Noisy Circle Grid, identical to circle## 
vars.noisycircle <- as_tibble(expand.grid(measure = "time", data.type = "annulus",
                                     data.dimensions = 2:4, num.points = seq(50, 500, 50),
                                     feature.dimensions = 1:3, 
                                     TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                     num.iteration = 10)) %>% subset(feature.dimensions < data.dimensions)


##Not sure where we want to cap it here since we can go infinitiely high## 
vars.box <- as_tibble(expand.grid(measure = "time", data.type = "uniform",
                                     data.dimensions = 2:5, num.points = seq(50, 500, 50),
                                     feature.dimensions = 1:4, 
                                     TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                     num.iteration = 10)) %>% subset(feature.dimensions < data.dimensions)

#This grid is simple and easy 
vars.torus <- as_tibble(expand.grid(measure = "time", data.type = "torus",
                                    data.dimensions = 2, num.points = seq(50, 500, 50),
                                    feature.dimensions = 1, 
                                    TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                    num.iteration = 10)) %>% subset(feature.dimensions < data.dimensions)













