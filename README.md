## Comparing R packages for calculation of persistent homology
Eashwar V. Somasundaram, Shael E. Brown, Adam Litzler, Jacob G. Scott, Raoul R. Wadhwa

### Directory Structure

* **Functions.R:** R script that collects benchmarking data. The workhorse function is `TDA_bench`.
* **Cluster_data:** contains benchmarking data (in CSV format) from the high-performance computing node at the Cleveland Clinic's Lerner Research Institute.
* **Mac_data:** contains benchmarking data (in CSV format) from a local machine.
* **Cluster_Scripts:** R and Slurm scripts to collect benchmark data (stored under the *Cluster_data* directory).
* **Figures:** contains R scripts and the PNG files they generate (see subdirectories as well). A subset of the PNG files were included the final report.
* **Supplemental_Regression:** Supplement section code and data, most of which was used to determine if functions grow on the order of a power function or an exponential function.
* **tdabench-paper:** contains the final report (in R markdown).

### Dependencies

The following R packages (see report for version details) were used in this report.

* `readr`: read rectangular data.
* `ggplot2`: visualize data.
* `scatterplot3d`: visualize data.
* `recexcavAAR`: visualize data.
* `magick`: visualize data.
* `bench`: measuring R code execution time.
* `pryr`: measure R object size.
* `TDA`: calculating persistent homology.
* `TDAstats`: calculating persistent homology.

### Instructions to Reproduce Data

Video explanations can be found [here](https://tinyurl.com/TDABench).

The main function to benchmark is TDA_bench. It can take the following variables
TDA_bench(measure, data.type, data.dimensions, num.points,
          feature.dimensions, TDA.library, num.iteration). It is defined with the other functions in the Functions.R Script

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
\nNumber of data set points. It can be any number n

feature.dimensions = 0, 1, 2, ... n-1
Dimensional features to calculate

TDA.library = "stats", "Dionysus", "GUDHI", "GUDHIalpha"
Which TDA library to use. "stats" refers to TDAstats. 
"Dionysus" and "GUDHI" refer to TDA. GUDHIalpha calculates
alpha complexes instead of Rips complex. 

num.iterations = n
How many iterations for median execution time calculation

The Benchmark_Runtime.R script and Benchmark_Object_Size.R script enables one to reproduce our benchmarking scripts. The script will create a csv file (time.csv or mem.csv) in the home directory. After each benchmark is performed, a row of data is appended to the CSV. If the computer crashes, the code will read in the previously run data from the csv and not rerun the benchmark on already collected datasets. By default, the iteration number is set to 10. 

The "default data" for these scripts collected on our lab computer is "mem1.csv" (found in main directory) and "time_mac_desktop.csv" (found in Mac_data). In our paper, we presented data run on a High Powered Cluster. We ran a special parallel script to accomplish this. The folder Cluster_Scripts and Cluster_data respectively contain the scripts and data from this run. A sample .sb SBATCH file is also provided if the user would like to replicate the cluster run (SBatch file may need to be modified for different HPCs). 

The Folder Figures allows replication of the figures presented in the paper. Running the script "Final_Figures_Reproducible.R" allows for reproduction of the paper's figures. The code calls on the csv data from the Cluster. The folder Unrasterized_Images contains the raw ggplots. Further code in the Reproducible script uses raster functions to improve the appearance of the final figures, which are found in Final_Figures. The extra data from our lab mac run (which can be replicated by running the Benchmark_Runtime.R script) is processed in Extra_Mac_Figures.R and the figures are found in Extra_Mac_Figures folder. 

The folder Supplemental Regression contains a script to create a power regression (log-log linear regression) for Rips complexes and linear regression for Alpha complexes. The regression data for each figure is shown in an html file. 

The folder tda-bench paper contains the paper. 

We strongly recommend cloning the entire directory to reproduce the code. In summary:
Functions.R: Creates the functions used to generate point clouds and benchmark using persistent homology functions
Benchmark_Object_Size.R: benchmark object size benchmark on your computer
Benchmark_Runtime.R: benchmark average runtime on your computer

Cluster_data: Raw data from our HPC Run
Cluster_Scripts: Scripts run on the HPC

Figures: Code to reproduce figures and folders of figures from paper 

Mac_data: Runtime data from running Benchmark_Runtime.R on lab mac. Not shown in final paper
mem1.csv: object size benchmark created from Benchmark_Object_Size.R. This data is identical no matter what computer it is run on

Supplemental Regression: Code and Data to calculate regression constants for each figure

To see video guides explaining the background and how to reproduce the figures and data, see this Youtube playlist: https://www.youtube.com/playlist?list=PLekORtkMHJMkETZVHzqaL9KWUl3peNmIk


