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
vars.circle <- as_tibble(expand.grid(measure = "memory", data.type = "circle",
                                     data.dimensions = 2:4, num.points = seq(50, 500, 25),
                                     feature.dimensions = 1:3, 
                                     TDA.library = c("GUDHI", "GUDHIalpha"),
                                     num.iteration = 1, file.name = "mem_CCF.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Annulus Variables for time
vars.noisycircle <- as_tibble(expand.grid(measure = "memory", data.type = "annulus",
                                          data.dimensions = 2:4, num.points = seq(50, 500, 25),
                                          feature.dimensions = 1:3, 
                                          TDA.library = c("GUDHI", "GUDHIalpha"),
                                          num.iteration = 1, file.name = "mem_CCF.csv")) %>% subset(feature.dimensions < data.dimensions)


#Making Box Variables for time
vars.box <- as_tibble(expand.grid(measure = "memory", data.type = "uniform",
                                  data.dimensions = 2:5, num.points = seq(50, 500, 25),
                                  feature.dimensions = 1:4, 
                                  TDA.library = c("GUDHI", "GUDHIalpha"),
                                  num.iteration = 1, file.name = "mem_CCF.csv")) %>% subset(feature.dimensions < data.dimensions)

#Making Torus Variables for time
vars.torus <- as_tibble(expand.grid(measure = "memory", data.type = "torus",
                                    data.dimensions = 3, num.points = seq(50, 500, 25),
                                    feature.dimensions = 1:2, 
                                    TDA.library = c("GUDHI", "GUDHIalpha"),
                                    num.iteration = 1, file.name = "mem_CCF.csv")) %>% subset(feature.dimensions < data.dimensions)
####

####Assemble variables for time...and delete the ones my laptop can't handle####
#If a certain point threshold for a certain dim feature is reached, then calc fails independent of point cloud shape and dim
#Comment out this next session for the real deal
vars.all <- rbind(vars.circle, vars.noisycircle, vars.box, vars.torus)

#Remove 4D analysis over 75 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>75 
                       & vars.all$feature.dimensions ==4
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 3D analysis over 100 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>100 
                       & vars.all$feature.dimensions ==3
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove 2D analysis over 275 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>275 
                       & vars.all$feature.dimensions ==2
                       & vars.all$TDA.library == "GUDHI"), ]

#Remove >3D clouds for GUDHI ALpha
vars.all <- vars.all[!(vars.all$data.dimensions > 3 
                       & vars.all$TDA.library == "GUDHIalpha"), ]

#Reads in collected data so far
vars.tested <- read.csv("mem_CCF.csv")
colnames(vars.tested) <- c("measure", "data.type", "data.dimensions",
                           "num.points", "feature.dimensions", "TDA.library", "memory")

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




