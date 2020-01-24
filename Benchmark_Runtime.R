####calling libraries, scrips, and making function####
library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)

source("Functions.R")

####Making the parameters to test for time####
##Creating variables for measuring Time ## 
#Making Circle Variables for time
vars.circle <- as_tibble(expand.grid(measure = "time", data.type = "circle",
                                     data.dimensions = 2:4, num.points = seq(10, 500, 5),
                                     feature.dimensions = 1:3, 
                                     TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                     num.iteration = 10, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Annulus Variables for time
vars.noisycircle <- as_tibble(expand.grid(measure = "time", data.type = "annulus",
                                          data.dimensions = 2:4, num.points = seq(10, 500, 5),
                                          feature.dimensions = 1:3, 
                                          TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                          num.iteration = 10, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)


#Making Box Variables for time
vars.box <- as_tibble(expand.grid(measure = "time", data.type = "uniform",
                                  data.dimensions = 2:8, num.points = seq(10, 500, 5),
                                  feature.dimensions = 1:7, 
                                  TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                  num.iteration = 10, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Torus Variables for time
vars.torus <- as_tibble(expand.grid(measure = "time", data.type = "torus",
                                    data.dimensions = 3, num.points = seq(10, 500, 5),
                                    feature.dimensions = 1:2, 
                                    TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                    num.iteration = 10, file.name = "time.csv")) %>% subset(feature.dimensions < data.dimensions)

#Including low point counts for Rbox 
vars.box.spec <- as_tibble(expand.grid(measure = "time", data.type = "uniform",
                                  data.dimensions = 2:8, num.points = seq(10, 20, 5),
                                  feature.dimensions = 1:7, 
                                  TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                  num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)
####

####Assemble variables for time...and delete the ones my laptop can't handle####
#If a certain point threshold for a certain dim feature is reached, then calc fails independent of point cloud shape and dim
#Remove Points that are not needed to show growth of the curve
#Comment out this next session for the real deal
vars.all <- rbind(vars.circle, vars.noisycircle, vars.box, vars.torus)
vars.all <- rbind(vars.circle, vars.noisycircle, vars.box, vars.torus) %>% 
  subset(feature.dimensions != 1 | num.points %% 25 == 0) %>%
  subset(feature.dimensions != 2 | num.points %% 20 == 0) %>%
  subset(feature.dimensions != 3 | num.points %% 10 == 0)

vars.all <- rbind(vars.all, vars.box.spec) %>% distinct()

#Remove 5+D analysis over 25
vars.all <- vars.all[!(vars.all$num.points>25 
                       & vars.all$feature.dimensions >= 5), ]

#Remove 4D analysis over 75 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>75 
                       & vars.all$feature.dimensions ==4
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 3D analysis over 125 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>125 
                       & vars.all$feature.dimensions ==3
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 2D analysis over 280 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>280 
                       & vars.all$feature.dimensions ==2
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 4D analysis over 50 points for Dionysus
vars.all <- vars.all[!(vars.all$num.points>50 
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
vars.tested <- read.csv("time.csv")
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

write.csv(vars.all, "done.csv")



####

####Run Function for all variables ####
#For real deal, uncomment this section
#mapply(TDA_bench, vars.all$measure, vars.all$data.type,
#       vars.all$data.dimensions, vars.all$num.points,
#       vars.all$feature.dimensions, vars.all$TDA.library,
#       vars.all$num.iteration, vars.all$file.name)
####




