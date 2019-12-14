library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(ggplot2)

source("Functions.R")

#####Function to make enclosing radius for TDA GUDHI and Dionysus (makes runtime fairer)#####
enclosing_radius <- function(X){
  # function which finds radius beyond which no homology changes
  # X is a point cloud data frame
  d = dist(X)
  n = nrow(X)
  return(min(unlist(lapply(X = 1:(nrow(X) - 1), FUN = function(X){ return(max(d[(1+(X-1)*n-(X-1)*X/2):(X*n-X*(X+1)/2)])) }))))
}

#####alternate bench function
bench.enc <- function(pointdata, TDA.library, featdim, num.iterations) {
  rad <- enclosing_radius(pointdata)
  str.lib <- paste(TDA.library)
  # TDAstats
  if (TDA.library == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim, threshold = rad),
                 iterations = num.iterations)
    # TDA - Dionysus
  } else if (TDA.library == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = rad,
                          location = FALSE, library = "Dionysus"),
                 iterations = num.iterations)
    # TDA - GUDHI
  } else if (TDA.library == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = rad,
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
  time.list <- time[1,12]
  time.runs <- unlist(time.list[[1]])
  appnd <- tibble()
  row <- c(rad, time.runs, str.lib)
  row.appnd <- rbind(appnd, row)
  colnames(row.appnd) <- seq(1:ncol(row.appnd))
  return(row.appnd)
}


list <- replicate(100, unifcircle(2, 2), simplify = FALSE)
list.rad <- lapply(list, enclosing_radius)
list.rads <- do.call(rbind, list.rad)



libraries <- c("stats", "GUDHI")

data <- mapply(bench.enc, pointdata = list, TDA.library = libraries, 
               featdim = 1, num.iterations = 1, SIMPLIFY = F)


data.graph <- do.call(rbind, data)
colnames(data.graph) <- c("rad", "time", "lib")
data.graph$rad <- as.numeric(as.character(data.graph$rad))
data.graph$time <- as.numeric(as.character(data.graph$time))


fig.2 <- ggplot(data.graph, aes(x=rad, y=time)) + 
  geom_point() + 
  facet_wrap( ~ lib) + theme(panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             strip.background = element_blank(),
                             panel.border = element_rect(color = "black", fill = NA, size = 1),
                             legend.key = element_blank(),
                             plot.title = element_text(hjust = 0.5),
                             axis.line = element_blank()) 

fig.2



unifcircle(100, 3)

