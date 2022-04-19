# Initialization ---------------------------------------------------------------
rm(list = ls())
pkgs_to_load <-
  c('AMKAT', 'Matrix', 'stats', 'MASS',
    'doParallel', 'foreach', 'iterators', 'beepr',
    'sim1000G', 'LaplacesDemon', 'mvtnorm', 'PearsonDS',
    'dplyr', 'ggplot2', 'tidyr', 'viridis', 'cowplot',
    'microbenchmark')
lapply(X = pkgs_to_load, FUN = library, character.only = TRUE)

# Root and source script directories
dir_main <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir_src <- file.path(dir_main, 'source_scripts')

# Define intermediary directories and create any that are nonexistent
source(file.path(dir_src, 'define_directories.R'))

# Initialize comparison methods
source(file.path(dir_src, 'initialize_comparison_methods.R'))
source(file.path(dir_src, "define_plot_functions.R"))
source(file.path(dir_src, "define_plot_settings.R"))

RNGkind("L'Ecuyer-CMRG") # for set.seed with foreach/doParallel
seed_value <- 42

overwrite_existing_data <- FALSE # repeat completed simulations?
reprocess_existing_data <- FALSE # repeat formatting of simulated data?
overwrite_existing_plots <- FALSE # re-generate existing plots?
overwrite_existing_tables <- FALSE # re-generate existing tables?

# Prepare workspace for parallel computing
num_cores <- detectCores() - 1
local_cluster <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(local_cluster)


# Size -------------------------------------------------------------------------
# Continuous X
size_or_power <- 'size'
x_type <- 'cts'
source(file.path(dir_src, 'initialize_size_power.R'))
num_replicates <- 5000
num_permutations <- 1000
alpha <- 0.05
values_for_error_distribution <- c('normal', 'cauchy')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(50, 100)
values_for_num_x_variables <- c(500, 2000)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'compile_results_size_power.R'))

# Discrete X
size_or_power <- 'size'
x_type <- 'snp'
source(file.path(dir_src, 'initialize_size_power.R'))
num_replicates <- 5000
num_permutations <- 1000
alpha <- 0.05
values_for_error_distribution <- c('normal', 'cauchy')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(30, 50, 70)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'compile_results_size_power.R'))

# Power ------------------------------------------------------------------------

plot_ymax <- 1

# Continuous X
size_or_power <- 'power'
x_type <- 'cts'
source(file.path(dir_src, 'initialize_size_power.R'))
num_replicates <- 1000
num_permutations <- 1000
alpha <- 0.05
values_for_error_distribution <- c('normal', 'cauchy')
values_for_signal_strength <- c(1)
values_for_num_x_variables <- c(100)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(20, 30, 40, 50, 60)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'prepare_plotdata_power.R'))
source(file.path(dir_src, 'plot_results_power.R'))

values_for_num_x_variables <- c(500)
values_for_sample_size <- c(50, 75, 100, 125, 150)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'prepare_plotdata_power.R'))
source(file.path(dir_src, 'plot_results_power.R'))

values_for_error_distribution <- c('normal')
values_for_num_x_variables <- c(1000)
values_for_sample_size <- c(50, 100, 150, 200)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'prepare_plotdata_power.R'))
source(file.path(dir_src, 'plot_results_power.R'))

values_for_num_x_variables <- c(2000)
values_for_error_corr_strength <- c(0.5)
values_for_sample_size <- c(50, 100, 150, 200)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'prepare_plotdata_power.R'))
source(file.path(dir_src, 'plot_results_power.R'))


# Discrete X
size_or_power <- 'power'
x_type <- 'snp'
source(file.path(dir_src, 'initialize_size_power.R'))
num_replicates <- 1000
num_permutations <- 1000
alpha <- 0.05
values_for_error_distribution <- c('normal')
values_for_signal_strength <- c(1)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)

values_for_sample_size <- c(20, 30, 40, 50, 60, 70)
source(file.path(dir_src, 'simulation_size_power.R'))
source(file.path(dir_src, 'prepare_plotdata_power.R'))
source(file.path(dir_src, 'plot_results_power.R'))

# Adaptive Methodology: Variability Across Data Sets ----------------------
#  Assess performance of PhiMr filter and kernel selection across data replicates

# Continuous X
x_type <- 'cts'
source(file.path(dir_src, 'initialize_adaptive_across.R'))
source(file.path(dir_src, 'simulation_adaptive_across.R'))
source(file.path(dir_src, 'prepare_plotdata_adaptive_across.R'))
source(file.path(dir_src, 'plot_results_kernel_selection.R'))
source(file.path(dir_src, 'plot_results_feature_selection.R'))

# Discrete X
x_type <- 'snp'
source(file.path(dir_src, 'initialize_adaptive_across.R'))
source(file.path(dir_src, 'simulation_adaptive_across.R'))
source(file.path(dir_src, 'prepare_plotdata_adaptive_across.R'))
source(file.path(dir_src, 'plot_results_kernel_selection.R'))
source(file.path(dir_src, 'plot_results_feature_selection.R'))


# Adaptive Methodology: Variability Within Data Sets ----------------------
#  Assess distribution of proposed P-value estimator for AMKAT with PhiMr filter
#    In particular, explore joint effect of # of test statistics and # of perms
#  Assess variability of PhiMr filter on a fixed data set

# Continuous X
x_type <- 'cts'
source(file.path(dir_src, "initialize_adaptive_within.R"))
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0.5)
values_for_sample_size <- c(30)
values_for_num_x_variables <- c(100)
source(file.path(dir_src, 'simulation_adaptive_within.R'))
source(file.path(dir_src, 'process_data_adaptive_within.R'))
source(file.path(dir_src, 'plot_results_adaptive_within.R'))
values_for_sample_size <- c(50)
values_for_signal_density <- c('dense')
source(file.path(dir_src, 'simulation_adaptive_within.R'))
source(file.path(dir_src, 'process_data_adaptive_within.R'))
source(file.path(dir_src, 'plot_results_adaptive_within.R'))
values_for_signal_density <- c('sparse')
values_for_num_x_variables <- c(150, 200)
source(file.path(dir_src, 'simulation_adaptive_within.R'))
source(file.path(dir_src, 'process_data_adaptive_within.R'))
source(file.path(dir_src, 'plot_results_adaptive_within.R'))

# Discrete X
x_type <- 'snp'
source(file.path(dir_src, "initialize_adaptive_within.R"))
values_for_signal_density <- c('sparse')
values_for_error_corr_strength <- c(0.5)
values_for_sample_size <- c(50)
source(file.path(dir_src, 'simulation_adaptive_within.R'))
source(file.path(dir_src, 'process_data_adaptive_within.R'))
source(file.path(dir_src, 'plot_results_adaptive_within.R'))

values_for_signal_density <- c('dense')
values_for_error_corr_strength <- c(0)
source(file.path(dir_src, 'simulation_adaptive_within.R'))
source(file.path(dir_src, 'process_data_adaptive_within.R'))
source(file.path(dir_src, 'plot_results_adaptive_within.R'))



# Runtime benchmarking ----------------------------------------------------

source(file.path(dir_src, 'initialize_runtime_benchmarking.R'))
source(file.path(dir_src, 'simulation_runtime_benchmarking.R'))
source(file.path(dir_src, 'prepare_plotdata_runtime_benchmarking.R'))
source(file.path(dir_src, 'plot_results_runtime_benchmarking.R'))


# Termination -------------------------------------------------------------

# Unregister local cluster used for parallel computing
stopCluster(local_cluster)
