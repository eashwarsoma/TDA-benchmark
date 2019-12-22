####calling libraries, scrips, and making function####
library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(snowfall)

# add if statements to check for parameter validity

#####BOX DATA#####
# generates an n-dimension box (0, 1) of uniformly distributed points
unifbox <- function(num.points, data.dimensions) {
  # empty 
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  
  # adds a column of randomaly generated points 0 to 1 for how many ever dimensions specified
  for (i in 1:data.dimensions) {
    col <- runif(num.points, 0, 1)
    to.calc.hom[, i] <- col
  }
  
  return(to.calc.hom)
}

#####TORUS DATA#####
# very simple, uses native uniform torus function from TDA
torus <- function(num.points) {
  torusUnif(num.points, 1, 1)
}

#####UNIFORM CIRCLE DATA#####
# Uses the sphere picking tactic to make uniform distribution 
# Cite Marsaglia paper from Wolfram Alpha page
unifcircle <- function(num.points, data.dimensions) {
  # var that stores result (empty df setup)
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  #to.calc.hom <- as.data.frame(to.calc.hom)
  
  # returns 2-d circle data
  if (data.dimensions == 2) {
    angles <- runif(num.points, 0, 2*pi)
    to.calc.hom <- cbind(cos(angles), sin(angles))
  }
  
  # returns 3-d circle data
  if (data.dimensions == 3) {
    
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
      # generate valid x1 and x2
      x1 <- runif(1, -1, 1)
      x2 <- runif(1, -1, 1)
      while (x1 ^ 2 + x2 ^ 2 >= 1) {
        x1 <- runif(1, -1, 1)
        x2 <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      x <- 2 * x1 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      y <- 2 * x2 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      z <- 1 - 2 * (x1 ^ 2 + x2 ^ 2)
      
      # store into data frame
      to.calc.hom[curr_row, ] <- c(x, y, z)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  if (data.dimensions == 4) { #follows same principle as previous but with more parameters
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
      # generate valid w, x, y, z
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      z <- runif(1, -1, 1)
      w <- runif(1, -1, 1)
      while (x ^ 2 + y ^ 2 >= 1 |
             w ^ 2 + z ^ 2 >= 1) {
        w <- runif(1, -1, 1)
        x <- runif(1, -1, 1)
        y <- runif(1, -1, 1)
        z <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      temp <- sqrt((1 - x ^ 2 - y ^ 2) / (w ^ 2 + z ^ 2))
      x1 <- x
      x2 <- y
      x3 <- z * temp
      x4 <- w * temp
      
      # store into data frame
      to.calc.hom[curr_row, ] <- c(x1, x2, x3, x4)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  # return answer variable
  return(to.calc.hom)
}

#####NOISY CIRCLE DATA#####
# almost idential to unif circle, but all x and y coordinates are multiplied by a perturbance varying from .9 to 1.1
noisycircle <- function(num.points, data.dimensions,
                        noise.magnitude = 0.1) {
  # get non-noisy data
  to.calc.hom <- unifcircle(num.points, data.dimensions)
  
  # add noise
  for (curr.col in 1:data.dimensions) {
    noise <- runif(num.points, 1 - noise.magnitude,
                   1 + noise.magnitude)
    to.calc.hom[, curr.col] <- to.calc.hom[, curr.col] * noise
  }
  
  # return noisy data
  return(to.calc.hom)
}



#####bench time#####
# NB: maxscale = 5 is used in TDA package examples, so used here (no default)

# point data input is required. program that is calculated is based off text string
# dimensional features and iteration number for benchmark should also be specified
bench <- function(pointdata, TDA.library, featdim, num.iterations) {
  # TDAstats
  if (TDA.library == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim, threshold = 4),
                 iterations = num.iterations)
    # TDA - Dionysus
  } else if (TDA.library == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 4,
                          location = FALSE, library = "Dionysus"),
                 iterations = num.iterations)
    # TDA - GUDHI
  } else if (TDA.library == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 4,
                          location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # TDA - GUDHI (alpha complex)
  } else if (TDA.library == "GUDHIalpha") {
    time <- mark(alphaComplexDiag(pointdata, maxdimension = featdim,
                                  location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # none of the above
  } else {
    stop("Invalid TDA engine selected")
  }
  
  # row 1 column 12 is list of times
  return(time[1,12])
}

#####measure memory#####
#point data input is required. program that is calculated is based off 
#text string. Dimensional features and iteration number for benchmark 
#should also be specified. Uses rips filtration function
memory <- function(pointdata, TDA.library, feature.dimensions) { 
  if (TDA.library == "Dionysus") { 
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                               library = "Dionysus")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHI") {
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                               library = "GUDHI")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHIalpha") {
    filtrate <- alphaComplexFiltration(pointdata, 
                                       library = "GUDHI")
    size <- object_size(filtrate[[1]])
    
  } else {
    stop("Choose 'Dionysus', 'GUDHI', or 'GUDHIalpa'. Cannot use TDAstats")
  }
  
  return(size)
}

#####combined function#####
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
    row <- as_tibble()
    row <- c(str.measure, str.data.type, data.dimensions, num.points, 
             feature.dimensions, str.TDA.library, exec.time.list)
    return(row)
  } else if (measure == "memory") {
    mem.data <- memory(pointdata, TDA.library,
                       feature.dimensions)
    return(mem.data)
  } else stop("Select either 'memory' or 'time' as measurement")
}
####


####Making the parameters to test for time####
##Creating variables for measuring Time ## 
#Making Circle Variables for time
vars.circle <- as_tibble(expand.grid(measure = "time", data.type = "circle",
                                     data.dimensions = 2:4, num.points = seq(10, 500, 5),
                                     feature.dimensions = 1:3, 
                                     TDA.library = c("stats", "Dionysus", "GUDHI", "GUDHIalpha"),
                                     num.iteration = 10, file.name = "timeccf.csv")) %>% subset(feature.dimensions < data.dimensions)



####

####Assemble variables for time...and delete the ones my laptop can't handle####
#If a certain point threshold for a certain dim feature is reached, then calc fails independent of point cloud shape and dim
#Remove Points that are not needed to show growth of the curve
#Comment out this next session for the real deal
vars.all <- rbind(vars.circle) %>% 
  subset(feature.dimensions != 1 | num.points %% 25 == 0) %>%
  subset(feature.dimensions != 2 | num.points %% 20 == 0) %>%
  subset(feature.dimensions != 3 | num.points %% 10 == 0)

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

#Remove 2D analysis over 300 points for Gudhi
vars.all <- vars.all[!(vars.all$num.points>300 
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

####Set Up Parallel####
sfInit(parallel=TRUE, cpus=4, type = "SOCK")
sfExport( "vars.all", "bench", "memory", "noisycircle", "TDA_bench", "torus",
          "unifbox", "unifcircle")
sfLibrary(plyr)
sfLibrary(readr)
sfLibrary(dplyr)
sfLibrary(TDA)
sfLibrary(TDAstats)
sfLibrary(bench)
sfLibrary(pryr)
sfLibrary(snowfall)


result <- sfClusterApplyLB(seq_len(nrow(vars.all)),
                           function(x) do.call(TDA_bench,as.list(vars.all[x,])))

format.data <- do.call(rbind, result)
sfStop()


write.csv(format.data, "circle.csv")






