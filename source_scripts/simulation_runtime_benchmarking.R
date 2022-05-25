
# Simulate Data -----------------------------------------------------------

# Prepare workspace for parallel processing
iterator <- rep(0L, num_cores) # each worker performs a single iteration
chunk_size <- 1L
packages_to_pass_to_foreach <- c('foreach', 'AMKAT', 'microbenchmark')

for (n in values_for_sample_size) {
  for (p in values_for_num_x_variables) {

    data_filename_stem <-
      paste0('b', num_permutations, '_m', num_replicates, '_n', n, '_p', p)
    data_filepath <- function(join_string) {
      file.path(dir_data_runtime_benchmarking,
                paste0(data_filename_stem, '_', join_string, '.Rdata'))
    }
    all_files_exist <-
      sum(
        file.exists(data_filepath(c("amkat", "stat", "cov", "phimr", "kermat")))
        ) == 5

    # Check first whether data has already been simulated
    if (!all_files_exist | overwrite_existing_data) {

      # Initialize scenario
      source(file.path(dir_src, 'initialize_simulation_scenario.R'))

      # Simulate data and fit null model
      covariates <- simulateDataCovariates()
      x <- simulateDataX()
      y <- simulateDataY(x, covariates)
      covdim <- ncol(covariates)
      w_aug <- cbind(rep(1, times = n), covariates)
      hat_matrix <- w_aug %*% tcrossprod(solve(crossprod(w_aug, w_aug)), w_aug)
      null_residuals <- (diag(n) - hat_matrix) %*% y
      null_standard_errors <-
        diag(crossprod(y, null_residuals)) / (n - covdim - 1)

      # Perform benchmarking
      source(file.path(dir_src, 'perform_runtime_benchmarking.R'))

    }
  }
}