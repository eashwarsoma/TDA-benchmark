
#number of sets of points to be used. Increase max to change number of data points
#Change num.points to change the multiple of points used
#change max dim to calculate higher dim
max <- 4
data.list <- list() 
num.points <- 50
points.col <- cbind(seq(num.points, max*num.points, num.points))
maxdim <- 1

#generates circle data for the different sets of points
for (i in 1:max) {
  angles <- runif(i*num.points, 0, 2*pi)
  data.list[[i]] <- cbind(cos(angles), sin(angles), tan(angles), angles, angles^2)
}

#tibbles were easier to use for some reason
benchstat.data <- tibble()
benchstat.row <- tibble()
benchTDA.data <- tibble()
benchTDA.row <- tibble()
table.row <- tibble()
table.memory <- tibble()

#benchmarks homology operation for stats and TDA for each circle
for (i in 1:max) {
  benchstat.row <- mark(calculate_homology(data.list[[i]], dim=maxdim), min_iterations = 10)
  benchstat.data[i,1:13] <- rbind(benchstat.row)
  benchTDA.row <- mark(ripsDiag(data.list[[i]], maxscale = 5, location = FALSE, maxdimension = maxdim), min_iterations = 10)
  benchTDA.data[i,1:13] <- rbind(benchTDA.row)
}

for (i in 1:max) {
  table.row <- object_size(benchTDA.data[[10]][[i]][["diagram"]])
  table.memory[i,1] <- rbind(table.row)
}


#generates tables used for plotting. Only plotted memory and median time
#multipliers were to get ms and kB
runtime.data <-cbind(points.col, 
                     1000*(benchstat.data %>% select(median)), 
                     1000*(benchTDA.data %>% select(median)))
memory.data <-cbind(points.col, 
                     .001*(benchstat.data %>% select(mem_alloc)), 
                     .001*(benchTDA.data %>% select(mem_alloc)),
                      .001*(table.memory %>% select(V1))
                    )


#Plots
colnames(runtime.data) <- c("Points", "stats", "TDA")
colnames(memory.data) <- c("Points", "stats", "TDA", "TblMem")

ggplot(runtime.data, aes(points.col, y = value, color = "")) + 
  geom_point(aes(y = runtime.data[["stats"]], col = "TDAstats")) + 
  geom_point(aes(y = runtime.data[["TDA"]], col = "TDA-GUDHI")) +
  xlab("Number of Points") +
  ylab("Median Execution Time (ms)") +
  ggtitle("Comparing TDAstats and TDA-GUDHI Run Times") +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.18, .95),
        legend.direction = "horizontal")+
  theme(legend.background=element_blank())+
  theme(legend.key=element_blank())

ggplot(memory.data, aes(points.col, y = value, color = "")) + 
  geom_point(aes(y = memory.data[["stats"]], col = "TDAstats")) + 
  geom_point(aes(y = memory.data[["TDA"]], col = "TDA-GUDHI")) +
  geom_point(aes(y = memory.data[["TblMem"]], col = "TDA Object Size")) +
  xlab("Number of Points") +
  ylab("Memory ALlocation (kB)") +
  ggtitle("Comparing TDAstats and TDA-GUDHI Memory Allocation") +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.29, .98),
        legend.direction = "horizontal")+
  theme(legend.background=element_blank())+
  theme(legend.key=element_blank())


     