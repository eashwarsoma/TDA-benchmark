library(dplyr)
library(ggplot2)
library(reshape)
library(ggforce)

#Reading in the time data
data.time <- read.csv("/Users/Soma/Downloads/temp_incomp_data.csv", header = TRUE)
#Adding Column Names
colnames(data.time) <- c("measure.type", "point.cloud", "point.cloud.dim", 
                         "num.points", "feat.dim", "library", 
                         "time1", "time2", "time3", "time4", 
                         "time5", "time6", "time7",
                         "time8", "time9", "time10")
#Creating average and standard deviation columns for time data
data.time$avg.time <- apply(data.time[,7:16],1,mean)
data.time$std <- apply(data.time[,7:16],1,sd)
data.time$min.time <- apply(data.time[,7:16],1,min)
data.time$max.time <- apply(data.time[,7:16],1,max)

#Create Factor labels for libraries
data.time$library <- factor(data.time$library,
                            levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
                            labels = c("TDAstats", "Dionysus", "GUDHI Rips", "GUDHI Alpha"))

#Read in object size data
data.mem <- read.csv("/Users/Soma/Downloads/mem1.csv", header = FALSE)

#Name columns
colnames(data.mem) <- c("measure.type", "point.cloud", "point.cloud.dim", 
                        "num.points", "feat.dim", "library", "memory")

#Create Factor labels for libraries
data.mem$library <- factor(data.mem$library,
                            levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
                            labels = c("TDAstats", "Dionysus", "GUDHI Rips", "GUDHI Alpha"))


####Figure_1####
#Goal: overview showing speed differences across engines on a canonical shape.

#Selected torus data scanning for two dimensional features
data.fig.1 <- subset(data.time, point.cloud == "torus" & feat.dim == 2 & 
                       library != "GUDHI Alpha") 

#Point graph; X: Num.points; Y: Runtime; Color: TDA library
fig.1 <- ggplot(data.fig.1, aes(x=num.points, y=avg.time, color=library)) + 
  geom_point()+
  geom_errorbar(data.fig.1, mapping = aes(x=num.points, 
                                            ymin=avg.time - std, 
                                            ymax=avg.time + std)) +
  labs(color = "TDA Library",
       x = "Number of Points on Torus",
       y = "Average Run Time",
       title = "Rips Complex Run Times on 3D Torus Point Clouds",
       subtitle = "") 

#Editing the colors (Making everything blank)
fig.1 <- fig.1 + theme_classic() +
                 theme(legend.position = c(0.85, 0.5),
                              legend.title = element_text(size = 9),
                              legend.text = element_text(size = 7),
                              plot.title = element_text(hjust = 0.5),
                              axis.line = element_line(colour = "black"),
                              legend.text.align = 0,
                              legend.title.align = 0)

fig.1 

ggsave("./Figures/Unrasterized_Images/fig1.png", plot = fig.1,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)



####Figure_2####
#Goal Comparison of persistent homology calculation runtimes 
#across R packages for n-dimensional sphere (n = 2, 3, 4)

#Selected all boxes up to n-1 dimensional features
data.fig.2 <- subset(data.time, point.cloud == "circle" & library != "GUDHI Alpha")
data.fig.2 <- data.fig.2[(data.fig.2$point.cloud.dim - 1 == data.fig.2$feat.dim), ]

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Sphere
fig.2 <- ggplot(data.fig.2, aes(x=num.points, y=avg.time, color=library)) + 
  geom_point() +
  facet_wrap( ~ point.cloud.dim, ncol=3, scales = "free") + 
  geom_errorbar(data.fig.2, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "TDA Library",
       x = "Number of Points on N-Sphere",
       y = "Average Run Time",
       title = "Rips Complex Run Times For n-Dimensional Spheres",
       subtitle = "") +
scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
scale_y_continuous(limits=c(-50,1800))


#Editing the colors (Making everything blank)
fig.2 <- fig.2 + theme_classic() +
                 theme(legend.position = c(0.93, 0.50),
                              legend.title = element_text(size = 9),
                              legend.text = element_text(size = 7),
                              plot.title = element_text(hjust = 0.5),
                              legend.text.align = 0,
                              legend.title.align = 0,
                              strip.background = element_blank(),
                              strip.text.x = element_blank(),
                              axis.line=element_line(),
                              axis.text.x = element_text(angle=-45, hjust=.025))

fig.2

ggsave("./Figures/Unrasterized_Images/fig2.png", plot = fig.2,
       scale = 1, width = 8, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figure 3: Runtime of persistent homology calculation as a function of feature dimension####
#Select all annulus data for all dimension point cloud and n-1 dim features
data.fig.3 <- subset(data.time, point.cloud == "uniform" & 
                       point.cloud.dim == 8 & library != "GUDHI Alpha" & 
                       (num.points == 10 | num.points == 15| num.points == 20))
data.fig.3$num.points <- as.factor(data.fig.3$num.points)

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Annulus
fig.3 <- ggplot(data.fig.3, aes(x=feat.dim, y=avg.time, color=num.points)) + 
  geom_point() + facet_wrap(~library, ncol = 2) + 
  geom_errorbar(data.fig.3, mapping = aes(x=feat.dim, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "Number of \nPoints",
       x = "Feature Dimensions",
       y = "Average Run Time",
       title = "Run Times For Extracting Dimensional Features 
       on an 8 Dimensional Box",
       subtitle = "") + scale_x_continuous(limits=c(0, 8), breaks=seq(0,8,1))

#Editing the colors (Making everything blank)
fig.3 <- fig.3 + theme_classic() +
                 theme(legend.position = c(0.93, 0.50),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 7),
                             plot.title = element_text(hjust = 0.5),
                             legend.text.align = 0,
                             legend.title.align = 0,
                             strip.background = element_blank(),
                             strip.text.x = element_blank(),
                             axis.text.x = element_text(angle=0, hjust=.025))
fig.3
ggsave("./Figures/Unrasterized_Images/fig3.png", plot = fig.3,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figure 4: Runtime of persistent homology calculation as a function of point cloud####
#Goal: show how increasing dimension slows down the calculation time.

#Select all 3D annuluses and change feature dimension to a factor variable
data.fig.4 <- subset(data.time, point.cloud == "annulus" & feat.dim == 1 & 
                       (library == "GUDHI Rips" | library == "GUDHI Alpha"))
data.fig.4$point.cloud.dim <- as.factor(data.fig.4$point.cloud.dim)
data.fig.4 <- as.data.frame(data.fig.4)

#Point graph facet; X: Num.points; Y: Runtime; Color: point.cloud dim; Facet: library
fig.4 <- ggplot(data.fig.4, aes(x=num.points, y=avg.time, color=point.cloud.dim)) + 
  geom_point() + facet_wrap(~library, scales = "free") + 
  geom_errorbar(data.fig.4, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "Annulus \nDimension",
       x = "Number of Points",
       y = "Average Run Time",
       title = "Rips vs Alpha Complex Run Times For Extracting \n1 Dimensional Features on N-dimensional Annuluses",
       subtitle = "") + 
  scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.02, 12))

#Editing the colors (Making everything blank)
fig.4 <- fig.4 + theme_classic() +
                  theme(legend.position = c(0.95, 0.50),
                               legend.title = element_text(size = 9),
                               legend.text = element_text(size = 7),
                               plot.title = element_text(hjust = 0.5),
                               legend.text.align = 0,
                               legend.title.align = 0,
                               axis.text.x = element_text(angle=0, hjust=.025),
                               strip.background = element_blank(),
                               strip.text.x = element_text())

fig.4
ggsave("./Figures/Unrasterized_Images/fig4.png", plot = fig.4,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

####Figure 5: Object Size use of Rips complex vs alpha complex. Engine = GUDHI(alpha).#### 
#Goal: compare memory
data.fig.5a <- subset(data.mem, point.cloud.dim == 3 & feat.dim == 2)

#Point graph facet; X: Num.points; Y: Memory; Color: Library; Facet: Pointcloud
fig.5a <- ggplot(data.fig.5a, aes(x=num.points, y=memory, color=library)) + 
  geom_point() + facet_wrap(~point.cloud, scales = "free") + 
  labs(color = "Complex",
       x = "Number of Points",
       y = "Object Size",
       title = "Object Size of Boundary Matrix",
       subtitle = "")+ 
  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(0, 1.8e10), breaks = c(1e5, 5e9, 1e10, 1.5e10)) 

#Editing the colors (Making everything blank)
fig.5a <- fig.5a + theme_classic() +
                    theme(legend.position = c(1.10, 0.550),
                                 legend.title = element_text(size = 9),
                                 legend.text = element_text(size = 7),
                                 plot.title = element_text(hjust = 0.5),
                                 legend.text.align = 0,
                                 legend.title.align = 0,
                                 axis.text.x = element_text(angle=0, hjust=.025),
                                 strip.background = element_blank(),
                                 plot.margin = unit(c(1,3,1,1), "cm"),
                                 strip.text.x = element_blank())

fig.5a
ggsave("./Figures/Unrasterized_Images/fig5a.png", plot = fig.5a,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

#Show what Rips Object Size depends on
data.fig.5b <- subset(data.mem, library == "GUDHI Rips")
data.fig.5b$feat.dim <- as.factor(data.fig.5b$feat.dim)

#Point graph facet; X: Num.points; Y: Memory; Color: Feat.Dim; 
fig.5b <- ggplot(data.fig.5b, aes(x=num.points, y=memory, color=feat.dim)) + 
  geom_point() + 
  labs(color = "Feature \n Dimension",
       x = "Number of Points",
       y = "Object Size",
       title = "Object Size of Rips Complex Boundary Matrix",
       subtitle = "")+ 
  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(0, 1.8e10), 
                     labels = function(x) format(x, scientific = TRUE))

#Editing the colors (Making everything blank)
fig.5b <- fig.5b + theme_classic() +
  theme(legend.position = c(0.92, 0.550),
               legend.title = element_text(size = 9),
               legend.text = element_text(size = 7),
               plot.title = element_text(hjust = 0.5),
               legend.text.align = 0,
               legend.title.align = 0,
               axis.text.x = element_text(angle=0, hjust=.025),
               strip.background = element_blank(),
               strip.text.x = element_blank())

fig.5b
ggsave("./Figures/Unrasterized_Images/fig5b.png", plot = fig.5b,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

#Show what Alpha Complex Object Size depends on
data.fig.5c <- subset(data.mem, library == "GUDHI Alpha")
#This is necessary since Alpha Complex Object Size Function Did Not
#Calculate Feat Dimensions lower than Point Cloud Dim - 1 in all cases
#This subset is necessary to remove the extraneous conditions that
#were not passed on to the alpha complex object size
data.fig.5c <- subset(data.fig.5c, point.cloud.dim - feat.dim == 1)

data.fig.5c$feat.dim <- as.factor(data.fig.5c$feat.dim)


#Point graph facet; X: Num.points; Y: Memory; Color: Feat.Dim; 
fig.5c <- ggplot(data.fig.5c, aes(x=num.points, y=memory, color=feat.dim)) + 
  geom_point() + facet_wrap(~point.cloud, scales = "free") +
  labs(color = "Feature \n Dimension",
       x = "Number of Points",
       y = "Object Size",
       title = "Object Size of Alpha Complex Boundary Matrix",
       subtitle = "")+ 
  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(0, 1.24e6), 
                     labels = function(x) format(x, scientific = TRUE))

#Editing the colors (Making everything blank)
fig.5c <- fig.5c + theme_classic() + theme(
              legend.position = c(1.16, 0.550),
              legend.title = element_text(size = 9),
              legend.text = element_text(size = 7),
              plot.title = element_text(hjust = 0.5),
              legend.text.align = 0,
              legend.title.align = 0,
               #plot.title = element_text(hjust = 0.5),
               axis.text.x = element_text(angle=0, hjust=.025),
               strip.background = element_blank(),
               strip.text.x = element_blank(), 
               plot.margin = unit(c(1,3,1,1), "cm"),
                panel.spacing = unit(2, "lines"))

fig.5c
ggsave("./Figures/Unrasterized_Images/fig5c.png", plot = fig.5c,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


#####Figure 6 (for discussion) TDAstats vs GUDHIalpha for runtime.####
data.fig.6 <- subset(data.time, point.cloud.dim == 3 & feat.dim == 2 & 
                       (library == "TDAstats" | library == "GUDHI Alpha"))

#Point graph facet; X: Num.points; Y: time; Color: Library; Facet: Pointcloud
fig.6 <- ggplot(data.fig.6, aes(x=num.points, y=avg.time, color=library)) + 
  facet_wrap(~point.cloud, scales = "free") + 
  geom_errorbar(data.fig.6, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) + geom_point() +
  labs(color = "Library",
       x = "Number of Points",
       y = "Average Run Time (s)",
       title = "Rips (TDAstats) vs Alpha Complex (GUDHI) \nRun Times on 3D Point Clouds",
       subtitle = "") + 
  scale_x_continuous(limits=c(5,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.05, 46))

#Editing the colors (Making everything blank)
fig.6 <- fig.6 + theme_classic()
fig.6 <- fig.6 + theme(
  legend.position = c(1.18, 0.550),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 7),
  plot.title = element_text(hjust = 0.5),
  legend.text.align = 0,
  legend.title.align = 0,
  #plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle=0, hjust=.025),
  strip.background = element_blank(),
  strip.text.x = element_blank(), 
  plot.margin = unit(c(1,3,1,1), "cm"),
  panel.spacing = unit(2, "lines"))
fig.6

ggsave("./Figures/Unrasterized_Images/fig6.png", plot = fig.6,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


























