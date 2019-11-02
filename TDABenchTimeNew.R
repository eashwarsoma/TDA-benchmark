####calling libraries, scrips, and making function####
library(readr)
library(dplyr)
library(plyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

source("generate-data.R")
source("bench.R")
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
  
  # step 2 benchmark
  if (measure == "time") {
    exec.time <- bench(pointdata, TDA.library,
                       feature.dimensions, num.iteration)
    exec.time <- exec.time[[1,1]]
    row.apnd  <- as_tibble(cbind(str.measure, str.data.type, data.dimensions, num.points, 
                                    feature.dimensions, str.TDA.library, exec.time))
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
                                     num.iteration = 1, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Annulus Variables for time
vars.noisycircle <- as_tibble(expand.grid(measure = "time", data.type = "annulus",
                                          data.dimensions = 2:4, num.points = seq(50, 500, 25),
                                          feature.dimensions = 1:3, 
                                          TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                          num.iteration = 1, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)


#Making Box Variables for time
vars.box <- as_tibble(expand.grid(measure = "time", data.type = "uniform",
                                  data.dimensions = 2:5, num.points = seq(50, 500, 25),
                                  feature.dimensions = 1:4, 
                                  TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                  num.iteration = 1, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Torus Variables for time
vars.torus <- as_tibble(expand.grid(measure = "time", data.type = "torus",
                                    data.dimensions = 3, num.points = seq(50, 500, 25),
                                    feature.dimensions = 1:2, 
                                    TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                    num.iteration = 1, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)
####

####Assemble variables for time...and delete the ones my laptop can't handle####
#Comment out this next session for the real deal
vars.all <- rbind(vars.circle, vars.noisycircle, vars.box, vars.torus)

#Remove 4D circle, 3D analysis over 300 points for stats
vars.all <- vars.all[!(vars.all$data.type == "circle" & vars.all$num.points>300 
                           & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3 
                            & vars.all$TDA.library == "stats"), ]

#Remove 4D annulus, 3D analysis over 300 points for stats
vars.all <- vars.all[!(vars.all$data.type == "annulus" & vars.all$num.points>300 
                           & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3
                            & vars.all$TDA.library == "stats"), ]

#Remove 4D uniform, 3D analysis over 300 points for stats
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>300 
                           & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3
                             & vars.all$TDA.library == "stats"), ]

#Remove 5D uniform, 3D analysis over 300 points for stats
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>300 
                           & vars.all$data.dimensions ==5 & vars.all$feature.dimensions ==3
                            & vars.all$TDA.library == "stats"), ]

#Remove 5D uniform, 4D analysis over 100 points for stats
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>100 
                           & vars.all$data.dimensions ==5 & vars.all$feature.dimensions ==4
                            & vars.all$TDA.library == "stats"), ]

#Remove 3D circle, 2D analysis over 200 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "circle" & vars.all$num.points>200 
                       & vars.all$data.dimensions ==3 & vars.all$feature.dimensions ==2
                        & vars.all$TDA.library == "Dionysus"), ]

#Remove 3D annulus, 2D analysis over 200 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "annulus" & vars.all$num.points>200 
                       & vars.all$data.dimensions ==3 & vars.all$feature.dimensions ==2
                        & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D circle, 2D analysis over 200 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "circle" & vars.all$num.points>200 
                       & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==2
                         & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D annulus, 2D analysis over 200 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "annulus" & vars.all$num.points>200 
                       & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==2
                         & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D circle, 3D analysis over 100 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "circle" & vars.all$num.points>100 
                       & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3
                        & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D annulus, 3D analysis over 100 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "annulus" & vars.all$num.points>100 
                       & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3
                         & vars.all$TDA.library == "Dionysus"), ]

#Remove 4D uniform, 3D analysis over 100 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>100 
                       & vars.all$data.dimensions ==4 & vars.all$feature.dimensions ==3
                        & vars.all$TDA.library == "Dionysus"), ]

#Remove 5D uniform, 3D analysis over 100 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>100 
                       & vars.all$data.dimensions ==5 & vars.all$feature.dimensions ==3
                        & vars.all$TDA.library == "Dionysus"), ]

#Remove 5D uniform, 4D analysis over 70 points for Dionysus
vars.all <- vars.all[!(vars.all$data.type == "uniform" & vars.all$num.points>70 
                       & vars.all$data.dimensions ==5 & vars.all$feature.dimensions ==4
                        & vars.all$TDA.library == "Dionysus"), ]
####

####splitting table into 10 tables to manage for my computer and running code####
#Comment out this next session for the real deal
vars.all.1 <- vars.all[1:160, ]
vars.all.2 <- vars.all[161:320, ]
vars.all.3 <- vars.all[321:480, ]
vars.all.4 <- vars.all[481:640, ]
vars.all.5 <- vars.all[641:800, ]
vars.all.6 <- vars.all[801:960, ]
vars.all.7 <- vars.all[961:1120, ]
vars.all.8 <- vars.all[1121:1280, ]
vars.all.9 <- vars.all[1281:1440, ]
vars.all.10 <- vars.all[1441:1646, ]

mapply(TDA_bench, vars.all.4$measure, vars.all.4$data.type,
       vars.all.4$data.dimensions, vars.all.4$num.points,
       vars.all.4$feature.dimensions, vars.all.4$TDA.library,
       vars.all.4$num.iteration, vars.all.4$file.name)

mapply(TDA_bench, vars.all.3$measure, vars.all.3$data.type,
       vars.all.3$data.dimensions, vars.all.3$num.points,
       vars.all.3$feature.dimensions, vars.all.3$TDA.library,
       vars.all.3$num.iteration, vars.all.3$file.name)

mapply(TDA_bench, vars.all.1$measure, vars.all.1$data.type,
       vars.all.1$data.dimensions, vars.all.1$num.points,
       vars.all.1$feature.dimensions, vars.all.1$TDA.library,
       vars.all.1$num.iteration, vars.all.1$file.name)

mapply(TDA_bench, vars.all.2$measure, vars.all.2$data.type,
       vars.all.2$data.dimensions, vars.all.2$num.points,
       vars.all.2$feature.dimensions, vars.all.2$TDA.library,
       vars.all.2$num.iteration, vars.all.2$file.name)

mapply(TDA_bench, vars.all.5$measure, vars.all.5$data.type,
       vars.all.5$data.dimensions, vars.all.5$num.points,
       vars.all.5$feature.dimensions, vars.all.5$TDA.library,
       vars.all.5$num.iteration, vars.all.5$file.name)

mapply(TDA_bench, vars.all.6$measure, vars.all.6$data.type,
       vars.all.6$data.dimensions, vars.all.6$num.points,
       vars.all.6$feature.dimensions, vars.all.6$TDA.library,
       vars.all.6$num.iteration, vars.all.6$file.name)

mapply(TDA_bench, vars.all.7$measure, vars.all.7$data.type,
       vars.all.7$data.dimensions, vars.all.7$num.points,
       vars.all.7$feature.dimensions, vars.all.7$TDA.library,
       vars.all.7$num.iteration, vars.all.7$file.name)

mapply(TDA_bench, vars.all.8$measure, vars.all.8$data.type,
       vars.all.8$data.dimensions, vars.all.8$num.points,
       vars.all.8$feature.dimensions, vars.all.8$TDA.library,
       vars.all.8$num.iteration, vars.all.8$file.name)

mapply(TDA_bench, vars.all.9$measure, vars.all.9$data.type,
       vars.all.9$data.dimensions, vars.all.9$num.points,
       vars.all.9$feature.dimensions, vars.all.9$TDA.library,
       vars.all.9$num.iteration, vars.all.9$file.name)

mapply(TDA_bench, vars.all.10$measure, vars.all.10$data.type,
       vars.all.10$data.dimensions, vars.all.10$num.points,
       vars.all.10$feature.dimensions, vars.all.10$TDA.library,
       vars.all.10$num.iteration, vars.all.10$file.name)
####

####Run Function for all variables ####
#For real deal, uncomment this section
#mapply(TDA_bench, vars.all$measure, vars.all$data.type,
#       vars.all$data.dimensions, vars.all$num.points,
#       vars.all$feature.dimensions, vars.all$TDA.library,
#       vars.all$num.iteration, vars.all$file.name)
####





