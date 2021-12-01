
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AMKAT Simulation Study

<!-- badges: start -->
<!-- badges: end -->

The AMKAT_simstudy Rstudio project was used to generate the data and
associated plots and tables found in:

Brian Neal and Tao He. “An Adaptive Multivariate Kernel-Based Test for
Association with Multiple Quantitative Traits in High-Dimensional Data.”
Genetic Epidemiology (Not yet submitted).

Executing the simulations requires having the AMKAT package installed,
which can be found at github.com/brianpatrickneal/AMKAT

## Instructions

#### Executing the script ‘AMKAT_simulation_study.R’ in the main directory will generate all data and plots:

-   Data will be contained in the subdirectory ‘simulated_data’
-   Plots will be contained in the subdirectory ‘plots’

#### To generate a .tex file containing tables for size and power after the corresponding data has been generated, knit the file ‘create_latex_tables.Rnw’, located in the ‘tables’ subdirectory, making sure to knit using knitr instad of Sweave. The .tex file will be titled ‘create_latex_tables.tex’ and will be located in the ‘tables’ subdirectory. The LaTeX code contained in the file can be copied and inserted into another LaTeX document as desired.
