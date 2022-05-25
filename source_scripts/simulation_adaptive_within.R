
# Simulate Data -----------------------------------------------------------

# Prepare workspace for parallel computing
iterator <- rep(0L, num_replicates)
chunk_size <- as.integer(ceiling(num_replicates / num_cores))
packages_to_pass_to_foreach <- c('foreach', 'AMKAT')
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      for (signal_density in values_for_signal_density) {
        for (error_correlation_strength in values_for_error_corr_strength) {
          for (n in values_for_sample_size) {
            for (p in values_for_num_x_variables) {
              source(file.path(dir_src, 'initialize_simulation_scenario.R'))

              data_filepath <- file.path(dir_data_adaptive_within,
                                         paste0(scenario_filename_stem,
                                                '_raw.Rdata'))

              # Check first whether data has already been simulated
              if (!file.exists(data_filepath) | overwrite_existing_data) {
                source(file.path(dir_src, 'simulate_data_adaptive_within.R'))
                savelist <- c(
                  'test_stats', 'perm_stats', 'x', 'covariates',
                  'y', 'null_residuals', 'null_standard_errors',
                  'kernel_select_rates', 'feature_select_rates',
                  'test_stat_no_filter', 'perm_stats_no_filter',
                  'selected_kernels_no_filter', 'pvals_comparison_methods')
                save(list = savelist, file = data_filepath)
              }
            }
          }
        }
      }
    }
  }
}