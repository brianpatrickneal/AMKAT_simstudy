# Initialization ---------------------------------------------------------------
rm(list=ls())
libraries_to_load <-
  c('Matrix', 'stats', 'MASS', 'doParallel', 'foreach', 'iterators', 'beepr',
    'sim1000G', 'LaplacesDemon', 'mvtnorm', 'PearsonDS', 'dplyr', 'ggplot2',
    'tidyr', 'hrbrthemes', 'viridis', 'gridExtra', 'grid')
lapply(X = libraries_to_load, FUN = library, character.only = TRUE)

dir_main <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir_src <- paste0(dir_main, '/source_scripts')
dir_data_size_power <- paste0(dir_main, '/simulated_data/size_power')
dir_data_feature_kernel_selection <-
  paste0(dir_main, '/simulated_data/feature_kernel_selection')
dir_data_adaptive_stability <- paste0(dir_main,
                                      '/simulated_data/adaptive_stability')
dir_plots_feature_selection <- paste0(dir_main, '/plots/feature_selection')
dir_plots_kernel_selection <- paste0(dir_main, '/plots/kernel_selection')
dir_plots_power <- paste0(dir_main, '/plots/power')
dir_plots_adaptive_stability <- paste0(dir_main, '/plots/adaptive_stability')
dir_tables <- paste0(dir_main, '/tables')

source(paste0(dir_src, '/initialize_comparison_methods.R'))

# Size -------------------------------------------------------------------------
# Continuous X
size_or_power <- 'size'
alpha <- 0.05
num_permutations <- 1000
num_replicates <- 5000
x_type <- 'cts'
values_for_error_distribution <- c('normal', 'cauchy')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(50, 100)
values_for_num_x_variables <- c(500, 2000)
source(paste0(dir_src, '/simulation_size_power.R'))
source(paste0(dir_src, '/compile_results_size_power.R'))

# Discrete X
size_or_power <- 'size'
alpha <- 0.05
num_permutations <- 1000
num_replicates <- 5000
x_type <- 'snp'
values_for_error_distribution <- c('normal', 'cauchy')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(30, 50, 70)
source(paste0(dir_src, '/simulation_size_power.R'))
source(paste0(dir_src, '/compile_results_size_power.R'))

# Power ------------------------------------------------------------------------
# Continuous X
size_or_power <- 'power'
alpha <- 0.05
num_permutations <- 1000
num_replicates <- 1000
x_type <- 'cts'
values_for_error_distribution <- c('normal')
values_for_signal_strength <- c(1)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(50, 100, 150, 200)
values_for_num_x_variables <- c(300, 500, 1000, 2000)
source(paste0(dir_src, '/simulation_size_power.R'))
source(paste0(dir_src, '/compile_results_size_power.R'))
source(paste0(dir_src, '/plot_results_power.R'))

# Discrete X
size_or_power <- 'power'
alpha <- 0.05
num_permutations <- 1000
num_replicates <- 1000
x_type <- 'snp'
values_for_error_distribution <- c('normal')
values_for_signal_strength <- c(1)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(30, 50, 70)
source(paste0(dir_src, '/simulation_size_power.R'))
source(paste0(dir_src, '/compile_results_size_power.R'))
source(paste0(dir_src, '/plot_results_power.R'))

# Feature Selection, Kernel Selection ------------------------------------------
size_or_power <- 'power' # size not supported
num_replicates  <- 1000
x_type <- 'cts'
values_for_error_distribution <- c('normal')
values_for_signal_strength <- c(1)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0)
values_for_sample_size <- c(50, 100, 150, 200)
values_for_num_x_variables <- c(100, 300, 500, 1000)
source(paste0(dir_src, '/simulation_feature_kernel_selection.R'))
source(paste0(dir_src, '/plot_results_feature_selection.R'))
source(paste0(dir_src, '/plot_results_kernel_selection.R'))

x_type <- 'snp'
values_for_sample_size <- c(30, 40, 50, 60, 70)
source(paste0(dir_src, '/simulation_feature_kernel_selection.R'))
source(paste0(dir_src, '/plot_results_feature_selection.R'))
source(paste0(dir_src, '/plot_results_kernel_selection.R'))

# Adaptive Stability -----------------------------------------------------------
size_or_power <- 'power'
alpha <- 0.05
num_replicates <- 100
x_type <- 'cts'
values_for_error_distribution <- c('normal', 'cauchy')
values_for_signal_strength <- c(1)
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)
values_for_sample_size <- c(50)
values_for_num_x_variables <- c(100, 300)
source(paste0(dir_src, '/simulation_adaptive_stability.R'))
source(paste0(dir_src, '/plot_results_adaptive_stability.R'))
values_for_error_corr_strength <- c(0.5)
values_for_sample_size <- c(100)
values_for_num_x_variables <- c(500, 1000, 2000)
source(paste0(dir_src, '/simulation_adaptive_stability.R'))
source(paste0(dir_src, '/plot_results_adaptive_stability.R'))

x_type <- 'snp'
values_for_sample_size <- c(30, 70)
source(paste0(dir_src, '/simulation_adaptive_stability.R'))
source(paste0(dir_src, '/plot_results_adaptive_stability.R'))