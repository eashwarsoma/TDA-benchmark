library(plyr)
library(readr)
library(dplyr)
library(TDA)
library(TDAstats)
library(bench)
library(pryr)
library(ggplot2)
library(magick)

####Reading in Point Clouds and Cropping them####
torus <- image_read("./Figures/Unrasterized_Images/torus.png") %>% 
         image_scale("1000") %>% 
        image_crop("525x410+225+150")

circle <- image_read("./Figures/Unrasterized_Images/circle.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x450+225+125")

sphere <- image_read("./Figures/Unrasterized_Images/sphere.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x525+225+150")

annulus.2 <- image_read("./Figures/Unrasterized_Images/2annulus.png") %>% 
  image_scale("1200") %>% 
  image_crop("525x450+315+240")

annulus.3 <- image_read("./Figures/Unrasterized_Images/3annulus.png") %>% 
  image_scale("1400") %>% 
  image_crop("525x525+410+320")

square <- image_read("./Figures/Unrasterized_Images/square.png") %>% 
  image_scale("900") %>% 
  image_crop("525x450+175+75")

cube <- image_read("./Figures/Unrasterized_Images/cube.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x475+225+200")


####Figure_1####
#Read in Figure
fig1.unrast <- image_read("./Figures/Unrasterized_Images/fig1.png")
#Combine Figure and Image
fig.1.rast <- image_composite(fig1.unrast, torus, offset = "+25+125", gravity = "northeast")
#Write Out File
image_write(fig.1.rast, path = './Figures/Final_Figures/fig1.png', format = 'png')


####Figure_2####
#Read in Figure
fig2.unrast <- image_read("./Figures/Unrasterized_Images/fig2.png")
#Combine Figure and Image
fig2.rast1 <- circle %>% image_scale("200") %>%
                         image_composite(fig2.unrast, ., offset = "+3225+205", 
                              gravity = "northeast")

fig2.rast2 <- sphere %>% image_scale("200") %>%
                         image_composite(fig2.rast1, ., offset = "+1700+205", 
                  gravity = "northeast")

fig2.rast2

#Write Out File
image_write(fig2.rast2, path = './Figures/Final_Figures/fig2.png', format = 'png')

####Figure_5a####
fig5a.unrast <- image_read("./Figures/Unrasterized_Images/fig5a.png")
#Combine Figure and Images
fig5a.rast1 <- annulus.3 %>% image_scale("200") %>%
  image_composite(fig5a.unrast, ., offset = "+1875+175", 
                  gravity = "northeast")

fig5a.rast2 <- sphere %>% image_scale("190") %>%
  image_composite(fig5a.rast1, ., offset = "+720+175", 
                  gravity = "northeast")

fig5a.rast3 <- torus %>% image_scale("210") %>%
  image_composite(fig5a.rast2, ., offset = "+1865+850", 
                  gravity = "northeast")

fig5a.rast4 <- cube %>% image_scale("210") %>%
  image_composite(fig5a.rast3, ., offset = "+710+850", 
                  gravity = "northeast")

fig5a.rast4
#Write Out File
image_write(fig5a.rast4, path = './Figures/Final_Figures/fig5a.png', format = 'png')


####Figure_5c####
fig5c.unrast <- image_read("./Figures/Unrasterized_Images/fig5c.png")
#Combine Figure and Images
fig5c.rast1 <- annulus.3 %>% image_scale("150") %>%
  image_composite(fig5c.unrast, ., offset = "+1850+280", 
                  gravity = "northeast")

fig5c.rast2 <- sphere %>% image_scale("150") %>%
  image_composite(fig5c.rast1, ., offset = "+410+550", 
                  gravity = "northeast")

fig5c.rast3 <- torus %>% image_scale("150") %>%
  image_composite(fig5c.rast2, ., offset = "+1875+1300", 
                  gravity = "northeast")

fig5c.rast4 <- cube %>% image_scale("150") %>%
  image_composite(fig5c.rast3, ., offset = "+325+1375", 
                  gravity = "northeast")

fig5c.rast5 <- annulus.2 %>% image_scale("150") %>%
  image_composite(fig5c.rast4, ., offset = "+1850+775", 
                  gravity = "northeast")

fig5c.rast6 <- circle %>% image_scale("150") %>%
  image_composite(fig5c.rast5, ., offset = "+400+830", 
                  gravity = "northeast")

fig5c.rast7 <- square %>% image_scale("150") %>%
  image_composite(fig5c.rast6, ., offset = "+325+1910", 
                  gravity = "northeast")

image_write(fig5c.rast7, path = './Figures/Final_Figures/fig5c.png', format = 'png')

####Figure 6####
fig6.unrast <- image_read("./Figures/Unrasterized_Images/fig6.png")
#Combine Figure and Images
fig6.rast1 <- annulus.3 %>% image_scale("185") %>%
  image_composite(fig6.unrast, ., offset = "+1975+225", 
                  gravity = "northeast")

fig6.rast2 <- sphere %>% image_scale("185") %>%
  image_composite(fig6.rast1, ., offset = "+820+225", 
                  gravity = "northeast")

fig6.rast3 <- torus %>% image_scale("185") %>%
  image_composite(fig6.rast2, ., offset = "+1965+900", 
                  gravity = "northeast")

fig6.rast4 <- cube %>% image_scale("185") %>%
  image_composite(fig6.rast3, ., offset = "+810+900", 
                  gravity = "northeast")
fig6.rast4

image_write(fig6.rast4, path = './Figures/Final_Figures/fig6.png', format = 'png')




####Other Figures####
#As of now, these don't need rasterizing
fig3 <- image_read("./Figures/Unrasterized_Images/fig3.png") 
image_write(fig3, path = './Figures/Final_Figures/fig3.png', format = 'png')


fig4 <- image_read("./Figures/Unrasterized_Images/fig4.png") 
image_write(fig4, path = './Figures/Final_Figures/fig4.png', format = 'png')

fig5b <- image_read("./Figures/Unrasterized_Images/fig5b.png") 
image_write(fig5b, path = './Figures/Final_Figures/fig5b.png', format = 'png')

####Combine Figure 5 into one grid
library(png)
library(grid)
library(gridExtra)


plot1 <- readPNG('./Figures/Final_Figures/fig5a.png')
plot2 <- readPNG('./Figures/Final_Figures/fig5b.png')
plot3 <- readPNG('./Figures/Final_Figures/fig5c.png')

png("./Figures/Final_Figures/fig5t.png", width = 6, height = 4, 
    units = "in", res = 450)
grid.arrange(
             rasterGrob(plot2, interpolate = T), 
             rasterGrob(plot3, interpolate = T), 
             nrow=1)
dev.off()

####Cropping White Space####
#Crop white space for Intro Alpha
alpha <- image_read("./Figures/Final_Figures/Intro_Alpha.png") 
alpha <- image_crop(alpha, "2700x1040", gravity = "North")
alpha <- image_crop(alpha, "2700x710", gravity = "South")
image_write(alpha, path = './Figures/Final_Figures/Intro_Alpha.png', format = 'png')

#Crop white space for Figure 5t
fig5 <- image_read("./Figures/Final_Figures/fig5t.png") 
fig5 <- image_crop(fig5, "2700x1400", gravity = "North")
fig5 <- image_crop(fig5, "2700x1020", gravity = "South")
image_write(fig5, path = './Figures/Final_Figures/fig5t.png', format = 'png')




####Mac Data Figure 1####
#Read in Figure
fig1.unrast <- image_read("./Figures/Unrasterized_Images/fig1mac.png")
#Combine Figure and Image
fig.1.rast <- image_composite(fig1.unrast, torus, offset = "+25+125", gravity = "northeast")
#Write Out File
image_write(fig.1.rast, path = './Figures/Final_Figures/fig1mac.png', format = 'png')

####Mac Data Figure 2####
#Read in Figure
fig2.unrast <- image_read("./Figures/Unrasterized_Images/fig2mac.png")
#Combine Figure and Image
fig2.rast1 <- circle %>% image_scale("200") %>%
  image_composite(fig2.unrast, ., offset = "+2125+205", 
                  gravity = "northeast")

fig2.rast2 <- sphere %>% image_scale("200") %>%
  image_composite(fig2.rast1, ., offset = "+1100+205", 
                  gravity = "northeast")

#Write Out File
image_write(fig2.rast2, path = './Figures/Final_Figures/fig2mac.png', format = 'png')

####Mac Data Figure 3 and 4####
fig3 <- image_read("./Figures/Unrasterized_Images/fig3mac.png") 
image_write(fig3, path = './Figures/Final_Figures/fig3mac.png', format = 'png')


fig4 <- image_read("./Figures/Unrasterized_Images/fig4mac.png") 
image_write(fig4, path = './Figures/Final_Figures/fig4mac.png', format = 'png')

####Mac Data Figure 6####
fig6.unrast <- image_read("./Figures/Unrasterized_Images/fig6mac.png")
#Combine Figure and Images
fig6.rast1 <- annulus.3 %>% image_scale("185") %>%
  image_composite(fig6.unrast, ., offset = "+1875+325", 
                  gravity = "northeast")

fig6.rast2 <- sphere %>% image_scale("185") %>%
  image_composite(fig6.rast1, ., offset = "+940+325", 
                  gravity = "northeast")

fig6.rast3 <- torus %>% image_scale("185") %>%
  image_composite(fig6.rast2, ., offset = "+1875+875", 
                  gravity = "northeast")

fig6.rast4 <- cube %>% image_scale("185") %>%
  image_composite(fig6.rast3, ., offset = "+940+875", 
                  gravity = "northeast")


image_write(fig6.rast4, path = './Figures/Final_Figures/fig6mac.png', format = 'png')











