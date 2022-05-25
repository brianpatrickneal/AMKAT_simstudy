# Default simulation parameters
size_or_power <- 'power'
alpha <- 0.05
num_replicates <- 500
values_for_error_distribution <- error_distribution <- 'normal'
values_for_signal_strength <- signal_strength <- 1
if (x_type == 'cts') {
  values_for_snp_signal_corr <- 'high'
}
if (x_type == 'snp') {
  values_for_num_x_variables <- p <- 567
}
num_permutations <- 5000L # per replicate
num_test_statistics <- 512L # per replicate
Q_values <- c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
B_values <- c(100, 200, 500, 1000, 2000, 3000, 4000, 5000)