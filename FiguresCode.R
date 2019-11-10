library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(ggplot2)
library(magick)

data.all <- read.csv("time1.csv", header = FALSE)

colnames(data.all) <- c("measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", "exec.time")


####Figure_1####
#Goal: overview showing speed differences across engines on a canonical shape.

#Selected torus data scanning for two dimensional features
data.fig.1 <- subset(data.all, point.cloud == "torus" & feat.dim == 2)

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


#Save image as png for easier photo editing
ggsave("fig.1.png", plot = fig.1)

#creating the magick image
torus <- image_read("torus.png")
pic.fig.1 <- image_read("fig.1.png")
pic.fig.1 <- image_resize(pic.fig.1, "1000x1000")

#fixing them together
image_composite(pic.fig.1, torus, offset = "+25+75", gravity = "northeast")



####Figure_2####
#Goal Comparison of persistent homology calculation runtimes across R packages for n-dimensional boxes (n = 2, 3, 4, 5). 

#Selected all boxes up to n-1 dimensional features
data.fig.2 <- subset(data.all, point.cloud == "uniform")
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

#Save image as png for easier photo editing
ggsave("fig.2.png", plot = fig.2)

#creating the square magick image
square <- image_read("square.png")
pic.fig.2 <- image_read("fig.2.png")

#fixing square to iamge
comp.fig.2.1 <- image_composite(pic.fig.2, square, offset = "+1150+200", gravity = "northeast")

#fixing cube to iamge
cube <- image_read("cube.png")
comp.fig.2.2 <- image_composite(comp.fig.2.1, cube, offset = "+375+175", gravity = "northeast")
comp.fig.2.2


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




