
# Simulate Data -----------------------------------------------------------

# Prepare workspace for parallel computing
iterator <- rep(0L, num_replicates)
chunk_size <- as.integer(ceiling(num_replicates / num_cores))
packages_to_pass_to_foreach <- c('foreach', 'Matrix', 'stats', 'MASS', 'AMKAT')
if (x_type == 'snp') {
  packages_to_pass_to_foreach <- c(packages_to_pass_to_foreach, 'sim1000G')
}

for (error_distribution in values_for_error_distribution) {
  if (error_distribution == 'cauchy') {
    packages_to_pass_to_foreach <- c(packages_to_pass_to_foreach,
                                     'LaplacesDemon')
  }
  for (signal_strength in values_for_signal_strength) {
    for (signal_density in values_for_signal_density) {
      for (error_correlation_strength in values_for_error_corr_strength) {
        for (n in values_for_sample_size) {
          for (p in values_for_num_x_variables) {

            data_filename_stem <-
              paste0('m', num_replicates, '_', x_type, '_', error_distribution,
                     '_s', signal_strength * 100, '_', signal_density,
                     '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            data_filepath <- file.path(dir_data_adaptive_across,
                                       paste0(data_filename_stem, '.Rdata'))

            # Check first whether data has already been simulated
            if (!file.exists(data_filepath) | overwrite_existing_data) {

              source(file.path(dir_src, 'initialize_simulation_scenario.R'))

              source(file.path(dir_src, 'simulate_data_adaptive_across.R'))

              save(list = c('selected_kernels', 'selected_x_columns'),
                   file = data_filepath)
            }
          }
        }
      }
    }
  }
}