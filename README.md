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

Video explanations can be found [here](https://tinyurl.com/TDABench). Video 1-4 describe the background and theory for persistent homology. Video 5 explains how to reproduce the figures and analysis using our data. Video 6 explains how to generate the data by performing the benchmarking on your machine. 



