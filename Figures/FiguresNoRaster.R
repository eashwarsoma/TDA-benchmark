library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(ggplot2)
library(magick)


data.time <- read.csv("time1.csv", header = FALSE)
data.mem <- read.csv("mem1.csv", header = FALSE)

colnames(data.time) <- c("measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", "exec.time")
colnames(data.mem) <- c("measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", "memory")


####Figure_1####
#Goal: overview showing speed differences across engines on a canonical shape.

#Selected torus data scanning for two dimensional features
data.fig.1 <- subset(data.time, point.cloud == "torus" & feat.dim == 2)

#Point graph; X: Num.points; Y: Runtime; Color: TDA library
fig.1 <- ggplot(data.fig.1, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() +
  labs(color = "TDA Library",
       x = "Number of Points on Torus",
       y = "Execution Time",
       title = "Per. Homology Run Times For 3D Torus",
       subtitle = "")

#Editing the colors (Making everything blank)
fig.1 <- fig.1 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       legend.position = c(0.87, 0.5),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_line(colour = "black"))

fig.1



####Figure_2####
#Goal Comparison of persistent homology calculation runtimes across R packages for n-dimensional boxes (n = 2, 3, 4, 5). 

#Selected all boxes up to n-1 dimensional features
data.fig.2 <- subset(data.time, point.cloud == "uniform")
data.fig.2 <- data.fig.2[(data.fig.2$point.cloud.dim - 1 == data.fig.2$feat.dim), ]

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Box
fig.2 <- ggplot(data.fig.2, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + 
  facet_wrap( ~ point.cloud.dim, ncol=2) + 
  labs(color = "TDA Library",
       x = "Number of Points on N-Box",
       y = "Execution Time",
       title = "Run Times For N-Dim Boxes",
       subtitle = "")

#Editing the colors (Making everything blank)
fig.2 <- fig.2 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       strip.background = element_blank(),
                       strip.text = element_blank(),
                       panel.border = element_rect(color = "black", fill = NA, size = 1),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())

fig.2

####Figure 3: Runtime of persistent homology calculation as a function of underlying engine (4d noisy circle/annulus)####
#Goal: show that TDAstats and GUDHIalpha are sufficiently faster than GUDHI and Dionysus, so the latter pair can be ignored for rest of figures.

#Select all annulus data for all dimension point cloud and n-1 dim features
data.fig.3 <- subset(data.time, point.cloud == "annulus")
data.fig.3 <- data.fig.3[(data.fig.3$point.cloud.dim - 1 == data.fig.3$feat.dim), ]

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Annulus
fig.3 <- ggplot(data.fig.3, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + facet_grid(cols = vars(point.cloud.dim))

#Editing the colors (Making everything blank)
fig.3 <- fig.3 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       strip.background = element_blank(),
                       strip.text = element_blank(),
                       panel.border = element_rect(color = "black", fill = NA, size = 1),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())
fig.3

####Figure 4: Runtime of persistent homology calculation as a function of underlying engine and engine####
#Goal: show how increasing dimension slows down the calculation time.

#Select all 4D circles and change feature dimension to a factor variable
data.fig.4 <- subset(data.time, point.cloud == "circle" & point.cloud.dim == 4)
data.fig.4$feat.dim <- as.factor(data.fig.4$feat.dim)

#Point graph facet; X: Num.points; Y: Runtime; Color: feat.dim; Facet: library
fig.4 <- ggplot(data.fig.4, aes(x=num.points, y=exec.time, color=feat.dim)) + 
  geom_point() + facet_grid(cols = vars(library))

#Editing the colors (Making everything blank)
fig.4 <- fig.4 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(color = "black", fill = NA, size = 1),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())

fig.4

#Figure 5: Memory use of Rips complex vs alpha complex. Engine = GUDHI(alpha). 
#Panel 1 = 4-sphere. Panel 2 = 3-annulus. Panel 3 = torus. Panel 4 = 5-box. 
#For each panel: point color/shape = engine (GUDHI vs alpha); 
#horizontal axis = number of points; 
#vertical axis = memory use; 
#feature dimension = data dimension - 1 for all panels. 
#Goal: compare memory
#POINT OUT THAT RIPSER DOESN'T MEASURE MEMORY B/C NO BOUNDARY MATRIX, BUT LIKELY SOMEWHERE IN BETWEEN THE TWO
data.fig.5 <- subset(data.mem, point.cloud.dim == 3 & feat.dim == 2)

#Point graph facet; X: Num.points; Y: Runtime; Color: feat.dim; Facet: library
fig.5 <- ggplot(data.fig.5, aes(x=num.points, y=memory, color=library)) + 
  geom_point() + facet_grid(cols = vars(point.cloud))

#Editing the colors (Making everything blank)
fig.5 <- fig.5 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(color = "black", fill = NA, size = 1),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())

fig.5




#Figure 6: Same as Figure 5, but TDAstats vs GUDHIalpha for runtime.
data.fig.6 <- subset(data.time, point.cloud.dim == 3 & feat.dim == 2 & (library == "stats" | library == "GUDHIalpha"))

#Point graph facet; X: Num.points; Y: Runtime; Color: feat.dim; Facet: library
fig.6 <- ggplot(data.fig.6, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + facet_wrap(~point.cloud)

#Editing the colors (Making everything blank)
fig.6 <- fig.6 + theme(panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(color = "black", fill = NA, size = 1),
                       legend.key = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.line = element_blank())

fig.6




