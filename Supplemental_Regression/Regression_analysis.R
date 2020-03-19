library(dplyr)
library(ggplot2)
library(reshape)
library(ggforce)
library(knitr)

####Reading in Data####
#Reading in the time data
data.time <- rbind(read.csv("./Cluster_data/annulus.csv", header = TRUE),
                   read.csv("./Cluster_data/circle.csv", header = TRUE),
                   read.csv("./Cluster_data/torus.csv", header = TRUE),
                   read.csv("./Cluster_data/uniform.csv", header = TRUE))
#Adding Column Names
colnames(data.time) <- c("row", "measure.type", "point.cloud", "point.cloud.dim", 
                         "num.points", "feat.dim", "library", 
                         "time1", "time2", "time3", "time4", 
                         "time5", "time6", "time7",
                         "time8", "time9", "time10")

#Remove extraneaous row variable
data.time <- data.time %>% select(-"row")

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
data.mem <- read.csv("./mem1.csv", header = FALSE)

#Name columns
colnames(data.mem) <- c("measure.type", "point.cloud", "point.cloud.dim", 
                        "num.points", "feat.dim", "library", "memory")

#Create Factor labels for libraries
data.mem$library <- factor(data.mem$library,
                           levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
                           labels = c("TDAstats", "Dionysus", "GUDHI Rips", "GUDHI Alpha"))

####Making Data Subsets####
#Data subsets identical to the data subset for each figure
data.fig.1 <- subset(data.time, point.cloud == "torus" & feat.dim == 2 & 
                       library != "GUDHI Alpha") 

data.fig.2 <- subset(data.time, point.cloud == "circle" & library != "GUDHI Alpha")
data.fig.2 <- data.fig.2[(data.fig.2$point.cloud.dim - 1 == data.fig.2$feat.dim), ]

data.fig.3 <- subset(data.time, point.cloud == "uniform" & 
                       point.cloud.dim == 8 & library != "GUDHI Alpha" & 
                       (num.points == 10 | num.points == 15| num.points == 20))
data.fig.3$num.points <- as.factor(data.fig.3$num.points)

data.fig.4 <- subset(data.time, point.cloud == "annulus" & feat.dim == 1 & 
                       (library == "GUDHI Rips" | library == "GUDHI Alpha"))
data.fig.4$point.cloud.dim <- as.factor(data.fig.4$point.cloud.dim)
data.fig.4 <- as.data.frame(data.fig.4)

data.fig.5a <- subset(data.mem, point.cloud.dim == 3 & feat.dim == 2)
data.fig.5b <- subset(data.mem, library == "GUDHI Rips")
data.fig.5b$feat.dim <- as.factor(data.fig.5b$feat.dim)
data.fig.5c <- subset(data.mem, library == "GUDHI Alpha")
data.fig.5c$feat.dim <- as.factor(data.fig.5c$feat.dim)

data.fig.6 <- subset(data.time, point.cloud.dim == 3 & feat.dim == 2 & 
                       (library == "TDAstats" | library == "GUDHI Alpha"))



####Regression####
#Create Rips Power Reg Function
#Returns coefficient, exponent, and R^2 from log log linear regression
reg.fit.rip <- function (data, strat, y, x) {
  #subset the data
  data.sub <- split(data, data[strat])
  data.sub <- data.sub[lapply(data.sub, nrow) != 0]
  
  #lin.regression to have a list of m and b predict values
  fmla.lin <- as.formula(paste("log(", y, ")"," ", "~", " ", "log(", x, ")", sep=""))
  lin.list <- lapply(data.sub, lm, formula = fmla.lin)
  #return(lin.list)
  #Extract the relevant coefficients
  lin.coef <- lapply(lin.list, function(z) c(z$coefficients, summary(z)$r.squared))
  #return(lin.coef)
  #Return a nice table in the for
  bval <- unlist(lapply(lin.coef, function(z) exp(z[1])))
  zval <- unlist(lapply(lin.coef, function(z) z[2]))
  rval <- unlist(lapply(lin.coef, function(z) z[3]))
  vals <- cbind(coef = bval, exp = zval, `R^2` = rval)
  row.names(vals) <- names(data.sub)
  return(vals)
}

#Create Alpha Linear Reg Function
#Returns coefficient, exponent, and R^2 from linear regression
reg.fit.alp <- function (data, strat, y, x) {
  #subset the data
  data.sub <- split(data, data[strat])
  data.sub <- data.sub[lapply(data.sub, nrow) != 0]
  
  #lin.regression to have a list of m and b predict values
  fmla.lin <- as.formula(paste(y," ", "~", " ", x, sep=""))
  lin.list <- lapply(data.sub, lm, formula = fmla.lin)
  #return(lin.list)
  #Extract the relevant coefficients
  lin.coef <- lapply(lin.list, function(z) c(z$coefficients, summary(z)$r.squared))
  #return(lin.coef)
  #Return a nice table in the for
  bval <- unlist(lapply(lin.coef, function(z) z[1]))
  zval <- unlist(lapply(lin.coef, function(z) z[2]))
  rval <- unlist(lapply(lin.coef, function(z) z[3]))
  vals <- cbind(Int = bval, Slope = zval, `R^2` = rval)
  row.names(vals) <- names(data.sub)
  return(vals)
}


#Figure 1
fig.1.reg.rip <- reg.fit.rip(data = data.fig.1,
                         strat = "library",
                         y = "avg.time",
                         x = "num.points")
kable(fig.1.reg.rip)

#Figure 2
fig.2.reg.rip <- reg.fit.rip(data = data.fig.2,
                         strat = c("library", "point.cloud.dim"),
                         y = "avg.time",
                         x = "num.points")
rownames(fig.2.reg.rip) <- c("TDAstats Circle",
                             "Dionysus Circle",
                             "GUDHI Circle",
                             "TDAstats Sphere",
                             "Dionysus Sphere",
                             "GUDHI Sphere",
                             "TDAstats Hypersphere",
                             "Dionysus Hypersphere",
                             "GUDHI Hypersphere")
kable(fig.2.reg.rip)


#Figure 3 (#exclude, data set is too small to tell reliable)
#This ggplot shows log(time) vs feat dim, does not follow exponential pattern
ggplot(data.fig.3, aes(x=feat.dim, y=log(avg.time), color=num.points, shape=num.points)) + 
  geom_point() + facet_wrap( ~ library)

#This ggplot shows log(time) vs log(feat dim), does not follow polynomial pattern either
ggplot(data.fig.3, aes(x=log(feat.dim), y=log(avg.time), color=num.points, shape=num.points)) + 
  geom_point() + facet_wrap( ~ library)
#For this reason, no regression is shown for this data

#Figure 4
fig.4.reg.rip <- reg.fit.rip(data = subset(data.fig.4, library == "GUDHI Rips"),
                         strat = "point.cloud.dim",
                         y = "avg.time",
                         x = "num.points")
rownames(fig.4.reg.rip) <- c("2-annulus",
                             "3-annulus",
                             "4-annulus")

fig.4.reg.alp <- reg.fit.alp(data = subset(data.fig.4, library == "GUDHI Alpha"),
                             strat = "point.cloud.dim",
                             y = "avg.time",
                             x = "num.points")
rownames(fig.4.reg.alp) <- c("2-annulus",
                             "3-annulus")

kable(fig.4.reg.rip, digits = 9)
kable(fig.4.reg.alp)

#Figure 5a
fig.5a.reg.rip <- reg.fit.rip(data = subset(data.fig.5a, library == "GUDHI Rips"),
                             strat = "point.cloud",
                             y = "memory",
                             x = "num.points")

fig.5a.reg.alp <- reg.fit.alp(data = subset(data.fig.5a, library == "GUDHI Alpha"),
                             strat = "point.cloud",
                             y = "memory",
                             x = "num.points")

kable(fig.5a.reg.rip)
kable(fig.5a.reg.alp)

#Figure 5b
fig.5b.reg.rip <- reg.fit.rip(data = subset(data.fig.5b, feat.dim != 4),
                             strat = "feat.dim",
                             y = "memory",
                             x = "num.points")
rownames(fig.5b.reg.rip) <- c("1-dim", "2-dim", "3-dim")

kable(fig.5b.reg.rip)

#Figure 5c
fig.5c.reg.alp <- reg.fit.alp(data = data.fig.5c,
                          strat = c("point.cloud", "point.cloud.dim"),
                          y = "memory",
                          x = "num.points")
rownames(fig.5c.reg.alp) <- c("2-annulus",
                             "Circle",
                             "2-box",
                             "3-annulus",
                             "sphere",
                             "torus",
                             "3-box")

kable(fig.5c.reg.alp)

#Figure 6
fig.6.reg.rip <- reg.fit.rip(data = subset(data.fig.6, library == "TDAstats"),
                              strat = "point.cloud",
                              y = "avg.time",
                              x = "num.points")

fig.6.reg.alp <- reg.fit.alp(data = subset(data.fig.6, library == "GUDHI Alpha"),
                              strat = "point.cloud",
                              y = "avg.time",
                              x = "num.points")

kable(fig.6.reg.rip, digits = 9)
kable(fig.6.reg.alp)

#Collecting all regressions into one list
all.reg.list <- list(
  fig.1.reg.rip,
  fig.2.reg.rip,
  fig.4.reg.rip,
  fig.4.reg.alp,
  fig.5a.reg.rip,
  fig.5a.reg.alp,
  fig.5b.reg.rip,
  fig.5c.reg.alp,
  fig.6.reg.rip,
  fig.6.reg.alp
)

names(all.reg.list) <- c("fig.1.reg.rip",
                           "fig.2.reg.rip",
                           "fig.4.reg.rip",
                           "fig.4.reg.alp",
                           "fig.5a.reg.rip",
                           "fig.5a.reg.alp",
                           "fig.5b.reg.rip",
                           "fig.5c.reg.alp",
                           "fig.6.reg.rip",
                           "fig.6.reg.alp")

#Converting everything to kable
list.reg <- lapply(all.reg.list, kable, format = "html")

#Writing to html
cat(list.reg[[1]], file = "./Supplemental_Regression/fig.1.reg.rip.html")
cat(list.reg[[2]], file = "./Supplemental_Regression/fig.2.reg.rip.html")
cat(list.reg[[3]], file = "./Supplemental_Regression/fig.4.reg.rip.html")
cat(list.reg[[4]], file = "./Supplemental_Regression/fig.4.reg.alp.html")
cat(list.reg[[5]], file = "./Supplemental_Regression/fig.5a.reg.rip.html")
cat(list.reg[[6]], file = "./Supplemental_Regression/fig.5a.reg.alp.html")
cat(list.reg[[7]], file = "./Supplemental_Regression/fig.5b.reg.rip.html")
cat(list.reg[[8]], file = "./Supplemental_Regression/fig.5c.reg.alp.html")
cat(list.reg[[9]], file = "./Supplemental_Regression/fig.6.reg.rip.html")
cat(list.reg[[10]], file = "./Supplemental_Regression/fig.6.reg.alp.html")





