# Simulation Parameters
num_replicates  <- 1000
num_permutations <- 1000
alpha <- 0.05
values_for_error_distribution <- c('normal')
values_for_signal_strength <- signal_strength <- 1
values_for_signal_density <- c('sparse', 'dense')
values_for_error_corr_strength <- c(0, 0.5)
size_or_power <- 'power'
if (x_type == 'cts') {
  values_for_snp_signal_corr <- NA
  values_for_sample_size <- c(50, 100, 150, 200)
  values_for_num_x_variables <- c(100, 300, 500, 1000)
}
if (x_type == 'snp') {
  values_for_snp_signal_corr <- c('low', 'high')
  values_for_sample_size <- c(50, 100, 150, 200)
  values_for_num_x_variables <- p <- 567
}


# Plotting Parameters
kernel_list <- c('gau', 'lin', 'quad', 'exp')
if (x_type == 'snp') kernel_list <- c(kernel_list, 'IBS')
y_col_labels <- c('Y1', 'Y2', 'Y3', 'Y4')
num_signals <- rep(0L, length(values_for_signal_density))
for (i in seq_along(values_for_signal_density)) {
  if (x_type == 'cts') num_signals[[i]] <-
      switch(values_for_signal_density[[i]], 'sparse' = 7L, 'dense' = 80L)
  if (x_type == 'snp') num_signals[[i]] <-
      switch(values_for_signal_density[[i]], 'sparse' = 28L, 'dense' = 122L)
}
# Values of p to include in plots for kernel selection (one column per value)
if (x_type == 'cts') pset_kersel_plots <- c(100, 500, 1000) # use 3 values