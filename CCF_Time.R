####calling libraries, scrips, and making function####
library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

source("generate-data.R")
source("CCF_bench.R")
source("MemorySize.R") 

TDA_bench <- function(measure, data.type, data.dimensions, num.points,
                      feature.dimensions, TDA.library, num.iteration, file.name) {
  print(paste("Starting", measure, data.type, data.dimensions, num.points,
              feature.dimensions, TDA.library, Sys.time()))
  
  str.measure <- paste(measure)
  str.data.type <- paste(data.type)
  str.TDA.library <- paste(TDA.library)
  str.file.name <- paste(file.name)
  
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
  
  # step 2 benchmark, write all times to a csv
  if (measure == "time") {
    exec.time <- bench(pointdata, TDA.library,
                       feature.dimensions, num.iteration)
    exec.time.list <- unlist(exec.time[[1]])
    row.apnd  <- as_tibble(cbind(str.measure, str.data.type, data.dimensions, num.points, 
                                 feature.dimensions, str.TDA.library, exec.time.list[1],
                                 exec.time.list[2], exec.time.list[3], exec.time.list[4],
                                 exec.time.list[5], exec.time.list[6], exec.time.list[7],
                                 exec.time.list[8], exec.time.list[9], exec.time.list[10]))
    write_csv(row.apnd, path = str.file.name, na = "NA", append = TRUE)
  } else if (measure == "memory") {
    mem.data <- memory(pointdata, TDA.library,
                       feature.dimensions)
    row.apnd  <- as_tibble(cbind(str.measure, str.data.type, data.dimensions, num.points, 
                                 feature.dimensions, str.TDA.library, mem.data))
    write_csv(row.apnd, path = str.file.name, na = "NA", append = TRUE)
  } else stop("Select either 'memory' or 'time' as measurement")
}
####


####Making the parameters to test for time####
##Creating variables for measuring Time ## 
#Making Circle Variables for time
vars.circle <- as_tibble(expand.grid(measure = "time", data.type = "circle",
                                     data.dimensions = 2:4, num.points = seq(50, 500, 25),
                                     feature.dimensions = 1:3, 
                                     TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                     num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Annulus Variables for time
vars.noisycircle <- as_tibble(expand.grid(measure = "time", data.type = "annulus",
                                          data.dimensions = 2:4, num.points = seq(50, 500, 25),
                                          feature.dimensions = 1:3, 
                                          TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                          num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)


#Making Box Variables for time
vars.box <- as_tibble(expand.grid(measure = "time", data.type = "uniform",
                                  data.dimensions = 2:5, num.points = seq(50, 500, 25),
                                  feature.dimensions = 1:4, 
                                  TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                  num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Torus Variables for time
vars.torus <- as_tibble(expand.grid(measure = "time", data.type = "torus",
                                    data.dimensions = 3, num.points = seq(50, 500, 25),
                                    feature.dimensions = 1:2, 
                                    TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                    num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)
####

####Assemble variables for time...and delete the ones my laptop can't handle####
#If a certain point threshold for a certain dim feature is reached, then calc fails independent of point cloud shape and dim
#Comment out this next session for the real deal
vars.all <- rbind(vars.circle, vars.noisycircle, vars.box, vars.torus)

#Remove 4D analysis over 100 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>100 
                       & vars.all$feature.dimensions ==4
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 3D analysis over 125 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>125 
                       & vars.all$feature.dimensions ==3
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 2D analysis over 300 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>300 
                       & vars.all$feature.dimensions ==2
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 4D analysis over 75 points for Dionysus
vars.all <- vars.all[!(vars.all$num.points>75 
                       & vars.all$feature.dimensions ==4
                       & vars.all$TDA.library == "Dionysus"), ]

#Remove 3D analysis over 100 points for Dionysus
vars.all <- vars.all[!(vars.all$num.points>100 
                       & vars.all$feature.dimensions ==3
                       & vars.all$TDA.library == "Dionysus"), ]

#Remove 2D analysis over 200 points for Dionysus
vars.all <- vars.all[!(vars.all$num.points>200 
                       & vars.all$feature.dimensions ==2
                       & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D analysis over 100 points for stats
vars.all <- vars.all[!(vars.all$num.points>100 
                       & vars.all$feature.dimensions ==4
                       & vars.all$TDA.library == "stats"), ]

#Remove 3D analysis over 300 points for stats
vars.all <- vars.all[!(vars.all$num.points>300 
                       & vars.all$feature.dimensions ==3
                       & vars.all$TDA.library == "stats"), ]

#Remove >3D objects for GUDHI ALpha
vars.all <- vars.all[!(vars.all$data.dimensions > 3 
                       & vars.all$TDA.library == "GUDHIalpha"), ]

#Reads in collected data so far
vars.tested <- read.csv("timeccf.csv")
colnames(vars.tested) <- c("measure", "data.type", "data.dimensions",
                           "num.points", "feature.dimensions", "TDA.library", "time1",
                           "time2", "time3", "time4", "time5", "time6", "time7", "time8",
                           "time9", "time10")

#From the variables, removes what has already been collected from each session
vars.tested$feature.dimensions <- as.integer(vars.tested$feature.dimensions)
vars.all <- vars.all %>% anti_join(vars.tested)

mapply(TDA_bench, vars.all$measure, vars.all$data.type,
       vars.all$data.dimensions, vars.all$num.points,
       vars.all$feature.dimensions, vars.all$TDA.library,
       vars.all$num.iteration, vars.all$file.name)

print("done")



####

####Run Function for all variables ####
#For real deal, uncomment this section
#mapply(TDA_bench, vars.all$measure, vars.all$data.type,
#       vars.all$data.dimensions, vars.all$num.points,
#       vars.all$feature.dimensions, vars.all$TDA.library,
#       vars.all$num.iteration, vars.all$file.name)
####




