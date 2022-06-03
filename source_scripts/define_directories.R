# Define intermediary directories

dir_simulated_data <-
  file.path(dir_main, 'simulated_data')

dir_data_size_power <-
  file.path(dir_simulated_data, 'size_power')

dir_plotdata_power <-
  file.path(dir_data_size_power, 'plot_data')

dir_data_adaptive_across <-
  file.path(dir_simulated_data, 'adaptive_across')

dir_data_adaptive_within <-
  file.path(dir_simulated_data, 'adaptive_within')

dir_data_runtime_benchmarking <-
  file.path(dir_simulated_data, 'runtime_benchmarking')

dir_tables <-
  file.path(dir_main, 'tables')

dir_plots <-
  file.path(dir_main, 'plots')

dir_plots_power <-
  file.path(dir_plots, 'power')

dir_plots_feature_selection <-
  file.path(dir_plots, 'adaptive_across', 'feature_selection')

dir_plots_kernel_selection <-
  file.path(dir_plots, 'adaptive_across', 'kernel_selection')

dir_plots_adaptive_within <-
  file.path(dir_plots, 'adaptive_within')

dir_plots_runtime_benchmarking <-
  file.path(dir_plots, 'runtime_benchmarking')

dir_plots_snp_corr_heatmaps <-
  file.path(dir_plots, 'snp_corr_heatmaps')


# Create any nonexistent directories
dirlist <- c(dir_simulated_data,
             dir_data_size_power,
             dir_data_adaptive_across,
             dir_data_adaptive_within,
             dir_data_runtime_benchmarking,
             dir_tables,
             dir_plots,
             dir_plots_power,
             dir_plots_feature_selection,
             dir_plots_kernel_selection,
             dir_plots_adaptive_within,
             dir_plots_runtime_benchmarking,
             dir_plots_snp_corr_heatmaps)
for (dir_i in dirlist) {
  if (!dir.exists(dir_i)) {
    dir.create(dir_i, showWarnings = FALSE, recursive = TRUE)
  }
}