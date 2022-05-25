# Simulation parameters
num_replicates <- 1L # number of executions for which to compute avg runtime
num_permutations <- 1000L # permutation statistics for estimating P-value
num_test_statistics <- 50L # for estimating AMKAT P-value with PhiMr filter
values_for_num_x_variables <- c(100, 200, 400, 800, 1600, 3200, 6400)
values_for_sample_size <- c(50, 100, 200, 400)
bench_kernels <- c('lin', 'quad','gau', 'IBS')

# Use default values for irrelevant parameters
size_or_power <- 'power'
alpha <- 0.05
x_type <- 'cts'
error_distribution <- 'normal'
signal_strength <- 1
signal_density <- 'sparse'
error_correlation_strength <- 0

# File for Plot data
bench_plotdata_file <- file.path(
  dir_data_runtime_benchmarking,
  paste0("b", num_permutations, "_m", num_replicates, ".Rdata"))

# Files for Plots
bench_plot_filename_stem <- paste0("b", num_permutations, "_m", num_replicates)
bench_file_kermat <-
  file.path(dir_plots_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_kermat.pdf"))
bench_file_stat <-
  file.path(dir_plots_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_stat.pdf"))
bench_file_phimr <-
  file.path(dir_plots_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_phimr.pdf"))
bench_file_amkat <-
  file.path(dir_plots_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_amkat.pdf"))
bench_file_cov <-
  file.path(dir_plots_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_cov.pdf"))

bench_plot_files_exist <-
  file.exists(bench_file_kermat) &
  file.exists(bench_file_stat) &
  file.exists(bench_file_phimr) &
  file.exists(bench_file_amkat)