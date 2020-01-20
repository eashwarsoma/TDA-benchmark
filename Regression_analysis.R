library(dplyr)
library(ggplot2)
library(reshape)
library(ggforce)
library(knitr)

source("./Figures/Creating_ggplot_Figures.R")


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

lapply(all.reg.list, kable, format = "latex")
