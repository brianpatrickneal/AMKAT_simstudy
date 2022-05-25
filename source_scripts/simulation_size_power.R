
# Simulate Data -----------------------------------------------------------

# Pseudocount adjustment value for permutation test P-values
pseudocount <- 1 / num_permutations

# Prepare workspace for parallel computing
iterator <- rep(0L, num_replicates)
chunk_size <- as.integer(ceiling(num_replicates / num_cores))
packages_to_pass_to_foreach <- c('foreach', 'Matrix', 'stats', 'MASS', 'AMKAT',
                                 'mvtnorm', 'PearsonDS')
if (x_type == 'snp') {
  packages_to_pass_to_foreach <- c(packages_to_pass_to_foreach, 'sim1000G')
}

for (error_distribution in values_for_error_distribution) {
  if (error_distribution == 'cauchy') {
    packages_to_pass_to_foreach <- c(packages_to_pass_to_foreach,
                                     'LaplacesDemon')
  }
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      for (signal_density in values_for_signal_density) {
        for (error_correlation_strength in values_for_error_corr_strength) {
          for (n in values_for_sample_size) {
            for (p in values_for_num_x_variables) {
              source(file.path(dir_src, 'initialize_simulation_scenario.R'))

              data_filepath <-
                file.path(dir_data_size_power,
                          paste0(scenario_filename_stem, '.Rdata'))

              # Check first whether data has already been simulated
              if (!file.exists(data_filepath) | overwrite_existing_data) {
                source(file.path(dir_src,
                                 paste0('simulate_data_',
                                        size_or_power, '_', x_type, '.R')))
                save(list = c('data', 'null_rejection_rates',
                              'comparison_pseudo_floor'),
                     file = data_filepath)
              }
            }
          }
        }
      }
    }
  }
}