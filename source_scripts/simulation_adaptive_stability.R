# ------------------------------------------------------------------------------
if (size_or_power == 'size') values_for_signal_strength <- 1
if (size_or_power == 'size') values_for_signal_density <- 'sparse'
if (x_type == 'snp') values_for_num_x_variables <- 567
num_permutations <- 1000L
pseudocount <- 1 / num_permutations
y_col_labels <- c('Y1', 'Y2', 'Y3', 'Y4')
method_labels <-
  c(paste0('AMKAT\nwithout filter\n', num_permutations, ' perms.'),
    paste0('AMKAT\nwith filter\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n10 obs. stats\n', num_permutations, ' perms.'),
    # paste0('AMKAT-MOSt\n20 obs. stats\n', num_permutations, ' perms.'),
    # paste0('AMKAT-MOSt\n50 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n100 obs. stats\n', num_permutations, ' perms.'),
    'AMKAT-MPv\n10 tests\n@100 perms.',
    # 'AMKAT-MPv\n20 tests\n@50 perms.', 'AMKAT-MPv\n50 tests\n@20 perms.',
    'AMKAT-MPv\n100 tests\n@10 perms.')
method_labels_avgstat <-
  c(paste0('AMKAT\nwith filter\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n10 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n20 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n50 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n100 obs. stats\n', num_permutations, ' perms.'))

# Function: take observed stats and permutation stats; divide them up
# and use them to generate multiple AMKAT P-values.
# NOTE: length(permutation_stats) is a multiple of length(observed_stats)
generatePValues <- function(observed_stats, permutation_stats) {
  num_tests <- length(observed_stats)
  batch_size <- length(permutation_stats) / num_tests
  p_values <- rep(NA, num_tests)
  for (i in seq_len(num_tests)) {
    observed_statistic <- observed_stats[[i]]
    start_index <- (i - 1) * batch_size + 1
    stop_index <- i * batch_size
    perm_stat_batch <- permutation_stats[start_index:stop_index]
    p_values[[i]] <- mean(observed_statistic <= perm_stat_batch)
  }
  return(p_values)
}
num_cores <- detectCores() - 1
iterator <- rep(0L, num_replicates)
chunk_size <- as.integer(ceiling(num_replicates / num_cores))
local_cluster <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(local_cluster)
packages_to_pass_to_foreach <- c('foreach', 'AMKAT')
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_density in values_for_signal_density) {
      for (error_correlation_strength in values_for_error_corr_strength) {
        for (n in values_for_sample_size) {
          for (p in values_for_num_x_variables) {
            source(paste0(dir_src, '/initialize_simulation_scenario.R'))
            source(paste0(dir_src, '/simulate_data_adaptive_stability.R'))
            savelist <- c(
              'observed_statistics', 'permutation_stats', 'x', 'covariates',
              'y', 'null_residuals', 'null_standard_errors',
              'kernel_select_rates', 'feature_select_rates',
              'test_stat_no_filter', 'permutation_stats_no_filter',
              'selected_kernels_no_filter', 'pvals_comparison_methods')
            save(list = savelist,
                 file = paste0(dir_data_adaptive_stability, '/',
                               scenario_filename_stem, '.Rdata'))
            source(paste0(dir_src, '/process_data_adaptive_stability.R'))
            save(list = c(savelist, 'pvalues_pseudo', 'pvalues_floor',
                          'pvalues_avgstat',),
                 file = paste0(dir_data_adaptive_stability, '/',
                               scenario_filename_stem, '.Rdata'))
          }
        }
      }
    }
  }
}
stopCluster(local_cluster)
beep()