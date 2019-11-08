library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(ggplot2)

data.all <- read.csv("time1.csv", header = FALSE)

colnames(data.all) <- c("measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", "exec.time")



#Figure 1: Comparison of persistent homology calculation runtimes across R packages for a torus. 
#Panel 2. Horizontal axis: number of points in torus. 
#Vertical axis: runtime. 
#Point chart. 
#Point color: underlying engine. 
#Point shape: same as color. 
#Panel 1. Torus showing users visual representation of dataset/point cloud. 
#Goal: overview showing speed differences across engines on a canonical shape.
data.fig.1 <- subset(data.all, point.cloud == "torus" & feat.dim == 2)
ggplot(data.fig.1, aes(x=num.points, y=exec.time, color=library)) + geom_point()



#Figure 2: Comparison of persistent homology calculation runtimes across R packages for n-dimensional boxes (n = 2, 3, 4, 5). 
#For each dimension, features up to n-1 dimensions are calculated. 
#Panel 1. Visual representation of 2-d box (maybe points, maybe box, maybe scattered both). 
#Panel 2. Visual representation of 3-d box (maybe points, maybe box, maybe both scattered). 
#Panels 3-6 (differing n={2,3,4,5} for dimension). 
#Point shape/color: underlying engine. 
#Horizontal axis: number of points. 
#Vertical axis: runtime.
data.fig.2 <- subset(data.all, point.cloud == "uniform")
data.fig.2 <- data.fig.2[(data.fig.2$point.cloud.dim - 1 == data.fig.2$feat.dim), ]
ggplot(data.fig.2, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + facet_grid(cols = vars(point.cloud.dim))

#Figure 3: Runtime of persistent homology calculation as a function of underlying engine (4d noisy circle/annulus). 
#Panel 1: visualize noisy circle - use imagination for 4d. 
#Panel 2. Horizontal axis: number of points. 
#Vertical axis: runtime. 
#Point color/shape: underlying engine. 
#Goal: show that TDAstats and GUDHIalpha are sufficiently faster than GUDHI and Dionysus, 
#so the latter pair can be ignored for rest of figures.
data.fig.3 <- subset(data.all, point.cloud == "annulus")
data.fig.3 <- data.fig.3[(data.fig.3$point.cloud.dim - 1 == data.fig.3$feat.dim), ]
ggplot(data.fig.3, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + facet_grid(cols = vars(point.cloud.dim))

#Figure 4: Runtime of persistent homology calculation as a function of 
#underlying engine and feature dimension (4d noisy circle/annulus). 
#Panel 1 = GUDHI alpha engine. 
#Horizontal axis = number of points. 
#Vertical axis = runtime. 
#Point color/shape = feature dimension. Panel 2 = panel 1, but with TDAstats engine. 
#Goal: show how increasing dimension slows down the calculation time.
data.fig.4 <- subset(data.all, point.cloud == "annulus" & point.cloud.dim == 4)
data.fig.4$feat.dim <- as.factor(data.fig.4$feat.dim)
ggplot(data.fig.4, aes(x=num.points, y=exec.time, color=feat.dim)) + 
  geom_point() + facet_grid(cols = vars(library))



#Figure 5: Memory use of Rips complex vs alpha complex. Engine = GUDHI(alpha). 
#Panel 1 = 4-sphere. Panel 2 = 3-annulus. Panel 3 = torus. Panel 4 = 5-box. 
#For each panel: point color/shape = engine (GUDHI vs alpha); 
#horizontal axis = number of points; 
#vertical axis = memory use; 
#feature dimension = data dimension - 1 for all panels. 
#Goal: compare memory
#POINT OUT THAT RIPSER DOESN'T MEASURE MEMORY B/C NO BOUNDARY MATRIX, BUT LIKELY SOMEWHERE IN BETWEEN THE TWO

#Figure 6: Same as Figure 5, but TDAstats vs GUDHIalpha for runtime.




