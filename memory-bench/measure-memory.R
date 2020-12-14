library(dplyr)

# setup data to collect
data_types <- c("circle", "annulus", "uniform", "torus")
dims <- 2:4
num_pts <- seq(from = 25, to = 250, by = 25)
engines <- c("TDAstats", "GUDHI", "Dionysus")
df <- expand.grid(data_types, dims, num_pts, engines,
                  stringsAsFactors = FALSE) %>%
  filter(!(Var1 == "torus" & Var2 != 3)) # torus has to have dimension 3

# output bash script to individualize sessions
vapply(X = seq_len(nrow(df)),
       FUN.VALUE = character(1),
       FUN = function(curr_row) {
         paste("Rscript",
               "individual-measure.R",
               paste(df[curr_row, ], collapse = " "))
       }) %>%
  # rep(10) %>%
  paste(collapse = "\n") %>%
  cat("\n", file = "measure-mem.sh")
