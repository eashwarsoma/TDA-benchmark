library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(scatterplot3d)
library(recexcavAAR)


source("generate-data.R")

#Create circle, zero the y so that the circle is looking normal
circle.2d <- unifcircle(200, 2)
circle.2d <- cbind(circle.2d, 0)
circle.2d <- circle.2d[,c(1,3,2)]

circle.2d.plot <- scatterplot3d(circle.2d, pch = 20, 
              grid=F, box=F,
              axis = F, angle = 0, highlight.3d = T)

#Create sphere
circle.3d <- unifcircle(900, 3)

circle.3d.plot <- scatterplot3d(circle.3d, pch = 20, 
                                grid=F, box=F,
                                axis = F, angle = 5, highlight.3d = T)


#Create Annulus, zero the y so that the circle is looking normal
annulus.2d <- noisycircle(400, 2)
annulus.2d <- cbind(annulus.2d, 0)
annulus.2d <- annulus.2d[,c(1,3,2)]

annulus.2d.plot <- scatterplot3d(annulus.2d, pch = 20, 
                                 highlight.3d = T,  
                                grid=F, box=F,
                                axis = F, angle = 0)

#Create 3d annulus
annulus.3d <- noisycircle(1300, 3)

annulus.3d.plot <- scatterplot3d(annulus.3d, pch = 20, 
                                grid=F, box=F,
                                axis = F, angle = 5, highlight.3d = T)

#Create torus, rotate it so it actually looks like a torus
torus.3d <-   torusUnif(2400, 1.8, 5)
torus.3d <- rotate(torus.3d[,1], torus.3d[,2], torus.3d[,3], 
                   degrx = 45, degry = 0, degrz = 0, 
                   pivotx = NA_real_,
                   pivoty = NA_real_, pivotz = NA_real_)

torus.3d.plot  <- scatterplot3d(torus.3d, pch = 20, 
                                highlight.3d = T,
                                 grid=F, box=F,
                                 axis = F, angle = 0)

#Create 2d annulus, add 0 to y axis to make it look normal
box.2d <- unifbox(350, 2)
box.2d <- cbind(box.2d, 0)
box.2d <- box.2d[,c(1,3,2)]

box.2d.plot <- scatterplot3d(box.2d, pch = 20, 
                                 highlight.3d = T,  
                                 grid=F, box=F,
                                 axis = F, angle = 0)

#Create 3d box
box.3d <- unifbox(4000, 3)
box.3d <- rotate(box.3d[,1], box.3d[,2], box.3d[,3], 
                   degrx = 30, degry = 60, degrz = 30, 
                   pivotx = NA_real_,
                   pivoty = NA_real_, pivotz = NA_real_)
box.3d.plot <- scatterplot3d(box.3d, pch = 20, 
                                 grid=F, box=F,
                                 axis = F, angle = 5, highlight.3d = T)

