library(dplyr)
library(ggplot2)
#Reading in the data
data.time <- rbind(read.csv("/Users/Soma/Downloads/torus.csv", header = TRUE),
                   read.csv("/Users/Soma/Downloads/circle.csv", header = TRUE))
data.mem <- read.csv("mem1.csv", header = FALSE)

#Adding Column Names
colnames(data.time) <- c("row", "measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", 
                         "time1", "time2", "time3", "time4", "time5", "time6", "time7",
                         "time8", "time9", "time10")
colnames(data.mem) <- c("measure.type", "point.cloud", "point.cloud.dim", "num.points", "feat.dim", "library", "memory")

#Creating average and standard deviation columns
#Remove extraneuous row variable
data.time <- data.time %>% select(-'row')

data.time$avg.time <- apply(data.time[,7:16],1,mean)

data.time$std <- apply(data.time[,7:16],1,sd)

data.time$min.time <- apply(data.time[,7:16],1,min)

data.time$max.time <- apply(data.time[,7:16],1,max)


####Figure_1####
#Goal: overview showing speed differences across engines on a canonical shape.

#Selected torus data scanning for two dimensional features
data.fig.1 <- subset(data.time, point.cloud == "torus" & feat.dim == 2 & 
                       library != "GUDHIalpha") %>%
              melt(id.vars = c("num.points", "library", "measure.type",
                               "feat.dim", "point.cloud", "point.cloud.dim",
                               "std", "avg.time", "min.time", "max.time"))


#Point graph; X: Num.points; Y: Runtime; Color: TDA library
fig.1 <- ggplot(data.fig.1, aes(x=num.points, y=value, color=library)) + 
  geom_point()+
  geom_errorbar(data.fig.1, mapping = aes(x=num.points, 
                                            ymin=avg.time - std, 
                                            ymax=avg.time + std)) +
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

##Model##
data.fig.1 %>% glm(avg.time ~ a*num.points^3, family = binomial)
model <- data.fig.1 %>% subset(library == "stats") %>% 
                        lm(avg.time ~ num.points*log(num.points), data = .)
summary(model)
plot(model)

data.fig.1 %>% subset(library == "Dionysus") %>% select("num.points", "avg.time")


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
  geom_point() + facet_grid(cols = vars(point.cloud.dim)) + 
  labs(color = "TDA Library",
       x = "Number of Points on N-Box",
       y = "Execution Time",
       title = "Run Times For N-Dim Annulus",
       subtitle = "")

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
  geom_point() + facet_grid(cols = vars(library)) + 
  labs(color = "feature dimension",
       x = "Number of Points",
       y = "Execution Time",
       title = "Run Times On 4D Sphere",
       subtitle = "")

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

####Figure 5: Memory use of Rips complex vs alpha complex. Engine = GUDHI(alpha). 
#Goal: compare memory
data.fig.5 <- subset(data.mem, point.cloud.dim == 3 & feat.dim == 2)

#Point graph facet; X: Num.points; Y: Memory; Color: Library; Facet: Pointcloud
fig.5 <- ggplot(data.fig.5, aes(x=num.points, y=memory, color=library)) + 
  geom_point() + facet_grid(cols = vars(point.cloud)) + 
  labs(color = "library",
       x = "Number of Points",
       y = "Object Size",
       title = "Object Size of Boundary Matrix",
       subtitle = "")

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




#####Figure 6: Same as Figure 5, but TDAstats vs GUDHIalpha for runtime.
data.fig.6 <- subset(data.time, point.cloud.dim == 3 & feat.dim == 2 & (library == "stats" | library == "GUDHIalpha"))

#Point graph facet; X: Num.points; Y: time; Color: Library; Facet: Pointcloud
fig.6 <- ggplot(data.fig.6, aes(x=num.points, y=exec.time, color=library)) + 
  geom_point() + facet_wrap(~point.cloud) + 
  labs(color = "library",
       x = "Number of Points",
       y = "Execution Time",
       title = "TDAstats vs GUDHI Alpha Complex",
       subtitle = "")

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


#####IntroFigures
library(ggtda)
source("Functions.R")

# generate a noisy circle
n <- 36; sd <- .2
set.seed(0)
t <- stats::runif(n = n, min = 0, max = 2*pi)
d <- data.frame(
  x = cos(t) + stats::rnorm(n = n, mean = 0, sd = sd),
  y = sin(t) + stats::rnorm(n = n, mean = 0, sd = sd)
)
# compute the persistent homology
ph <- as.data.frame(TDAstats::calculate_homology(as.matrix(d), dim = 1))
print(head(ph, n = 12))
#>    dimension birth      death
#> 1          0     0 0.02903148
#> 2          0     0 0.05579919
#> 3          0     0 0.05754819
#> 4          0     0 0.06145429
#> 5          0     0 0.10973364
#> 6          0     0 0.11006440
#> 7          0     0 0.11076601
#> 8          0     0 0.12968679
#> 9          0     0 0.14783527
#> 10         0     0 0.15895889
#> 11         0     0 0.16171041
#> 12         0     0 0.16548606
ph <- transform(ph, dim = as.factor(dimension))

# attach *ggtda*
library(ggtda)
#> Loading required package: ggplot2
# visualize disks of fixed radii and the Vietoris complex for this proximity
p_d <- ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3", alpha = .15) +
  geom_point()
p_sc <- ggplot(d, aes(x = x, y = y)) +
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod", alpha = .1) +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris0()
# combine the plots
gridExtra::grid.arrange(
  p_d, p_sc,
  layout_matrix = matrix(c(1, 2), nrow = 1)
)

#####test 
library(ggplot2)
library(TDAstats)
library(cowplot)

circle <- circle2d

mydata <- as.data.frame(circle)

ggplot(mydata, aes(x = V1, y = V2))  + 
  geom_point(size = I(25), alpha = .1, color = "aquamarine") +
  geom_point(color = I("black")) +
  theme_cowplot()




phom <- calculate_homology(mydata)


























