
# Load and Format Data ----------------------------------------------------

# Check first whether files already exist
if (!file.exists(bench_plotdata_file) | reprocess_existing_data) {

  # Initialize storage
  data_kermat <- array(data = NA,
                       dim = c(length(values_for_sample_size), # rows index n
                               length(values_for_num_x_variables), # cols index p
                               5 # third dim indexes kernel
                       ))
  dimnames(data_kermat) <- list(values_for_sample_size,
                                values_for_num_x_variables,
                                c("Lin", "Quad", "Gau", "Exp", "IBS"))

  data_phimr <- matrix(data = NA,
                       nrow = length(values_for_sample_size), # rows index n
                       ncol = length(values_for_num_x_variables), # cols index p
  )
  rownames(data_phimr) <- values_for_sample_size
  colnames(data_phimr) <- values_for_num_x_variables

  data_stat <- data_cov <- data_phimr

  data_amkat <- array(data = NA,
                      dim = c(length(values_for_sample_size), # rows index n
                              length(values_for_num_x_variables), # cols index p
                              2 # third dim indexes AMKAT method
                      ))
  dimnames(data_amkat) <- list(values_for_sample_size,
                               values_for_num_x_variables,
                               c("NoFilter", "PhiMr50"))

  # Total number of executions (to be read from data)
  reps_kermat <- reps_phimr <- reps_stat <- reps_cov <- 0

  for (n_ind in seq_along(values_for_sample_size)) {
    n <- values_for_sample_size[[n_ind]]
    for (p_ind in seq_along(values_for_num_x_variables)) {
      p <- values_for_num_x_variables[[p_ind]]

      # load data
      data_filename_stem <-
        paste0('b', num_permutations, '_m', num_replicates, '_n', n, '_p', p)
      data_filepath <- function(join_string) {
        file.path(dir_data_runtime_benchmarking,
                  paste0(data_filename_stem, '_', join_string, '.Rdata'))
      }
      load(data_filepath("amkat"))
      load(data_filepath("phimr"))
      load(data_filepath("stat"))
      load(data_filepath("cov"))
      load(data_filepath("kermat"))

      # Compute median times
      data_kermat[n_ind, p_ind, ] <-
        apply(times_kermat, MARGIN = 2, FUN = median)
      data_phimr[n_ind, p_ind] <- median(times_phimr)
      data_stat[n_ind, p_ind] <- median(times_stat)
      data_cov[n_ind, p_ind] <- median(times_cov)
      data_amkat[n_ind, p_ind, ] <-
        apply(times_amkat, MARGIN = 2, FUN = median)

      # Record total number of executions
      reps_kermat <- nrow(times_kermat)
      reps_phimr <- length(times_phimr)
      reps_stat <- length(times_stat)
      reps_cov <- length(times_cov)

    }
  }

  # Save formatted data
  save(data_kermat, data_phimr, data_amkat, data_stat, data_cov,
       reps_kermat, reps_phimr, reps_stat, reps_cov,
       file = bench_plotdata_file)

}
