# ------------------------------------------------------------------------------
if (size_or_power == 'size') values_for_signal_strength <- 1
if (size_or_power == 'size') values_for_signal_density <- 'sparse'
if (x_type == 'snp') values_for_num_x_variables <- 567

num_cores <- detectCores() - 1
iterator <- rep(0L, num_replicates)
chunk_size <- as.integer(ceiling(num_replicates / num_cores))
local_cluster <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(local_cluster)
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
    for (signal_density in values_for_signal_density) {
      for (error_correlation_strength in values_for_error_corr_strength) {
        for (n in values_for_sample_size) {
          for (p in values_for_num_x_variables) {
            source(paste0(dir_src, '/initialize_simulation_scenario.R'))
            source(paste0(dir_src, '/simulate_data_',
                          size_or_power, '_', x_type, '.R'))
            save(list = c('data', 'null_rejection_rates',
                          'comparison_pseudo_floor'),
                 file = paste0(dir_data_size_power, '/',
                               scenario_filename_stem, '.Rdata'))
          }
        }
      }
    }
  }
}
stopCluster(local_cluster)
beep()