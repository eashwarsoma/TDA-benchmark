This code measures execution time using the bench library and object size using the pryr library of persistent homology calculations using different TDA libraries. 

The main benchmark function is found in "FullTDABench.R" However, you should clone the whole repository since it sources the other scripts. "generate-data.R" creates different geometrically distributed point clouds, "bench.R" measures median execution time of a persistent homology calculation, and "MemorySize.R" measures memory size of the returned persistent homology n x 3 matrix. 

In the FullTDABench.R, the function is TDA_bench. It can take the following variables
TDA_bench(measure, data.type, data.dimensions, num.points,
          feature.dimensions, TDA.library, num.iteration)

measure = "time" or "memory"
Measures the median execution time of a persistent homology
calculation or object size of returned persistent homology
matrix

data.type = "circle", "uniform", "annulus", or "torus"
Generates data set for a uniform circle, uniform n dimensional box,
annulus, or 3D torus. 

data.dimensions = 2, 3, 4, ... n (data.type dependent)
Species dimensions of data. Circle and Annulus can have 2, 3, 4
dimensions. Uniform can take on any n dimensions. Torus can only
have 3 dimensions

num.points = n
Number of data set points. It can be any number n

feature.dimensions = 0, 1, 2, ... n-1
Dimensional features to calculate

TDA.library = "stats", "Dionysus", "GUDHI", "GUDHIalpha"
Which TDA library to use. "stats" refers to TDAstats. 
"Dionysus" and "GUDHI" refer to TDA. GUDHIalpha calculates
alpha complexes instead of Rips complex. 

num.iterations = n
How many iterations for median execution time calculation