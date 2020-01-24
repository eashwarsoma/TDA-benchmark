library(dplyr)
library(ggplot2)
library(reshape)
library(ggforce)


####Alternative Figures from Extra Mac Desktop Time Data####
#This data was run on a 16 GB lab mac computer
#While we did not present this data in the official paper
#We ran it just to be sure that the benchmark results were similar
#To the High powered cluster data

#Figure 5 was left out since object size is invariant and always the same  

#Reading in the time data
data.time.mac <- read.csv("./time_mac_desktop.csv", header = FALSE)
#Adding Column Names
colnames(data.time.mac) <- c("measure.type", "point.cloud", "point.cloud.dim", 
                         "num.points", "feat.dim", "library", 
                         "time1", "time2", "time3", "time4", 
                         "time5", "time6", "time7",
                         "time8", "time9", "time10")

#Creating average and standard deviation columns for time data
data.time.mac$avg.time <- apply(data.time.mac[,7:16],1,mean)
data.time.mac$std <- apply(data.time.mac[,7:16],1,sd)
data.time.mac$min.time <- apply(data.time.mac[,7:16],1,min)
data.time.mac$max.time <- apply(data.time.mac[,7:16],1,max)

#Create Factor labels for libraries
data.time.mac$library <- factor(data.time.mac$library,
                            levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
                            labels = c("TDAstats", "Dionysus", "GUDHI Rips", "GUDHI Alpha"))


####Figure 1 Mac####
data.fig.1.mac <- subset(data.time.mac, point.cloud == "torus" & feat.dim == 2 & 
                       library != "GUDHI Alpha") 

#Point graph; X: Num.points; Y: Runtime; Color: TDA library
fig.1.mac <- ggplot(data.fig.1.mac, aes(x=num.points, y=avg.time, color=library)) + 
  geom_point()+
  geom_errorbar(data.fig.1.mac, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "TDA Library",
       x = "Number of Points on Torus",
       y = "Average Run Time",
       title = "Rips Complex Run Times on 3D Torus Point Clouds",
       subtitle = "") 

#Editing the colors (Making everything blank)
fig.1.mac <- fig.1.mac + theme_classic() +
  theme(legend.position = c(0.85, 0.5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.title.align = 0)

ggsave("./Figures/Extra_Mac_Figures/fig1mac.png", plot = fig.1.mac,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

####Figure 2 Mac####
#Goal Comparison of persistent homology calculation runtimes 
#across R packages for n-dimensional sphere (n = 2, 3, 4)

#Selected all boxes up to n-1 dimensional features
data.fig.2.mac <- subset(data.time.mac, point.cloud == "circle" & library != "GUDHI Alpha")
data.fig.2.mac <- data.fig.2.mac[(data.fig.2.mac$point.cloud.dim - 1 == data.fig.2.mac$feat.dim), ]

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Sphere
fig.2.mac <- ggplot(data.fig.2.mac, aes(x=num.points, y=avg.time, color=library)) + 
  geom_point() +
  facet_wrap( ~ point.cloud.dim, ncol=3, scales = "free") + 
  geom_errorbar(data.fig.2.mac, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "TDA Library",
       x = "Number of Points on N-Sphere",
       y = "Average Run Time",
       title = "Rips Complex Run Times For n-Dimensional Spheres",
       subtitle = "") +
  scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-30,2100))


#Editing the colors (Making everything blank)
fig.2.mac <- fig.2.mac + theme_classic() +
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

ggsave("./Figures/Extra_Mac_Figures/fig2mac.png", plot = fig.2.mac,
       scale = 1, width = 8, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

####Figure 3 Mac####
data.fig.3.mac <- subset(data.time.mac, point.cloud == "uniform" & 
                       point.cloud.dim == 8 & library != "GUDHI Alpha" & 
                       (num.points == 10 | num.points == 15| num.points == 20))
data.fig.3.mac$num.points <- as.factor(data.fig.3.mac$num.points)

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Annulus
fig.3.mac <- ggplot(data.fig.3.mac, aes(x=feat.dim, y=avg.time, color=num.points)) + 
  geom_point() + facet_wrap(~library, scales = "free") + 
  geom_errorbar(data.fig.3.mac, mapping = aes(x=feat.dim, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "Number of \nPoints",
       x = "Feature Dimensions",
       y = "Average Run Time",
       title = "Run Times For Extracting Dimensional \nFeatures on an 8 Dimensional Box",
       subtitle = "") + scale_x_continuous(limits=c(0, 8), breaks=seq(0,8,1)) +
  scale_y_continuous(limits=c(-.01, 3))

#Editing the colors (Making everything blank)
fig.3.mac <- fig.3.mac + theme_classic() +
  theme(legend.position = c(0.93, 0.60),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.text.align = 0,
        legend.title.align = 0,
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.text.x = element_text(angle=0, hjust=.025))

ggsave("./Figures/Extra_Mac_Figures/fig3mac.png", plot = fig.3.mac,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

####Figure 4 Mac####
data.fig.4.mac <- subset(data.time.mac, point.cloud == "annulus" & feat.dim == 1 & 
                       (library == "GUDHI Rips" | library == "GUDHI Alpha"))
data.fig.4.mac$point.cloud.dim <- as.factor(data.fig.4.mac$point.cloud.dim)
data.fig.4.mac <- as.data.frame(data.fig.4.mac)

#Point graph facet; X: Num.points; Y: Runtime; Color: point.cloud dim; Facet: library
fig.4.mac <- ggplot(data.fig.4.mac, aes(x=num.points, y=avg.time, color=point.cloud.dim)) + 
  geom_point() + facet_wrap(~library, scales = "free") + 
  geom_errorbar(data.fig.4.mac, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "Annulus \nDimension",
       x = "Number of Points",
       y = "Average Run Time",
       title = "Rips vs Alpha Complex Run Times For Extracting \n1 Dimensional Features on N-dimensional Annuluses",
       subtitle = "") + 
  scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.02, 22))

#Editing the colors (Making everything blank)
fig.4.mac <- fig.4.mac + theme_classic() +
  theme(legend.position = c(0.95, 0.50),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.text.align = 0,
        legend.title.align = 0,
        axis.text.x = element_text(angle=0, hjust=.025),
        strip.background = element_blank(),
        strip.text.x = element_text())

ggsave("./Figures/Extra_Mac_Figures/fig4mac.png", plot = fig.4.mac,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figure 6 Mac####
data.fig.6.mac <- subset(data.time.mac, point.cloud.dim == 3 & feat.dim == 2 & 
                       (library == "TDAstats" | library == "GUDHI Alpha"))

#Point graph facet; X: Num.points; Y: time; Color: Library; Facet: Pointcloud
fig.6.mac <- ggplot(data.fig.6.mac, aes(x=num.points, y=avg.time, color=library)) + 
  facet_wrap(~point.cloud, scales = "free") + 
  geom_errorbar(data.fig.6.mac, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) + geom_point() +
  labs(color = "Library",
       x = "Number of Points",
       y = "Average Run Time (s)",
       title = "Rips (TDAstats) vs Alpha Complex (GUDHI) \nRun Times on 3D Point Clouds",
       subtitle = "") + 
  scale_x_continuous(limits=c(5,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.05, 84))

#Editing the colors (Making everything blank)
fig.6.mac <- fig.6.mac + theme_classic()
fig.6.mac <- fig.6.mac + theme(
  legend.position = c(1.13, 0.550),
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

ggsave("./Figures/Extra_Mac_Figures/fig6mac.png", plot = fig.6.mac,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)




