# --- Initialize Preliminary Objects -------------------------------------------
num_tests_at_10_perms_each <- num_permutations / 10
method_label_mpv10 <- paste0('AMKAT_MPv_10x', num_tests_at_10_perms_each)
chosen_method_names <-
  c('AMKAT_floor', 'AMKAT_avgstat_128_floor',
    paste0(method_label_mpv10, '_floor'), 'OMGA_residual_Y', 'DKAT', 'DK_L')
chosen_method_labels <-
  c('AMKAT', 'AMKAT-MOSt', 'AMKAT-MPv', 'OMGA', 'DKAT', 'DKAT-LinY')
if (size_or_power == 'size') {
  values_for_signal_strength <- NA
  values_for_signal_density <- NA
}
if (x_type == 'snp') values_for_num_x_variables <- 567
if (size_or_power == 'power') {
  num_signals <- rep(0L, length(values_for_signal_density))
  for (i in seq_along(values_for_signal_density)) {
    if (x_type == 'cts') {
      num_signals[[i]] <- switch(values_for_signal_density[[i]],
                                 'sparse' = 7L, 'regular' = 32L, 'dense' = 80L)
    }
    if (x_type == 'snp') {
      num_signals[[i]] <- switch(values_for_signal_density[[i]],
                                 'sparse' = 28L, 'dense' = 123L)
    }
  }
}
row_group_names <- paste0('$p = ', values_for_num_x_variables, '$')
col_group_names <- paste0('$n = ', values_for_sample_size, '$')
row_group_size <- length(chosen_method_names)
col_group_size <- length(values_for_sample_size)
num_row_groups <- length(values_for_num_x_variables)
num_col_groups <- length(values_for_error_corr_strength)
row_names <- rep(chosen_method_labels, num_row_groups)
col_names <- rep(col_group_names, num_col_groups)
row_group_index <- double()
for (i in 1:num_row_groups) {
  temp <- row_group_size
  names(temp) <- paste0('Feature dimension ' ,values_for_num_x_variables[[i]])
  row_group_index <- c(row_group_index, temp)
}
header_row <- c(' ' = 1)
for (i in 1:num_col_groups) {
  temp <- col_group_size
  if (values_for_error_corr_strength[[i]] == 0) {
    names(temp) <- 'Independent errors'
  }
  if (values_for_error_corr_strength[[i]] > 0 &
      values_for_error_corr_strength[[i]] < 0.5) {
    names(temp) <- 'Mildly correlated errors'
  }
  if (values_for_error_corr_strength[[i]] >= 0.5) {
    names(temp) <- 'Correlated errors'
  }
  header_row <- c(header_row, temp)
}
if (x_type == 'cts') xtype <- 'continuous'
if (x_type == 'snp') xtype <- 'discrete'
# Compile and export -----------------------------------------------------------
chunk_of_rows <-
  matrix(NA, nrow = length(chosen_method_names),
         ncol = (length(values_for_sample_size) *
                   length(values_for_error_corr_strength)))
for (signal_strength in values_for_signal_strength) {
  for (signal_density_index in seq_along(values_for_signal_density)) {
    signal_density <- values_for_signal_density[[signal_density_index]]
    for (error_distribution in values_for_error_distribution) {
      if (error_distribution == 'cauchy') error_distribution_label <- 'Cauchy'
      if (error_distribution == 'normal') error_distribution_label <- 'normal'
      if (size_or_power == 'size') {
        table_filename_stem <- paste0(
          size_or_power, '_a', alpha * 1000, '_b', num_permutations,
          '_m', num_replicates, '_', x_type, '_', error_distribution)
        caption <- paste0(
          'Empirical ', size_or_power, ' in percentage (over ', num_replicates,
          ' replicates) for each method testing at the ', alpha,
          ' significance level under scenarios with ', xtype, ' features and ',
          'multivariate ', error_distribution_label, ' random errors.')
      }
      if (size_or_power == 'power') {
        table_filename_stem <- paste0(
          size_or_power, '_a', alpha * 1000, '_b', num_permutations,
          '_m', num_replicates, '_', x_type, '_', error_distribution,
          '_s', signal_strength * 100, '_', signal_density)
        caption <- paste0(
          'Empirical ', size_or_power, ' in percentage (over ', num_replicates,
          ' replicates) for each method testing at the ', alpha,
          ' significance level under scenarios with ', xtype, ' features, ',
          num_signals[[signal_density_index]], 'signal variables, and ',
          'multivariate ', error_distribution_label, ' random errors. ')
      }
      compiled_results <- rep(NA, length(values_for_sample_size) *
                                length(values_for_error_corr_strength))
      for (p in values_for_num_x_variables) {
        for (i in seq_along(values_for_error_corr_strength)) {
          error_correlation_strength <- values_for_error_corr_strength[[i]]
          column_start_index <- (i - 1) * length(values_for_sample_size) + 1
          for (sample_size_index in seq_along(values_for_sample_size)) {
            n <- values_for_sample_size[[sample_size_index]]
            chunk_column_index <- column_start_index + (sample_size_index - 1)
            if (size_or_power == 'power') {
              scenario_filename_stem <- paste0(
                size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                '_m', num_replicates, '_', x_type, '_', error_distribution,
                '_s', signal_strength * 100, '_', signal_density,
                '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            }
            if (size_or_power == 'size') {
              scenario_filename_stem <- paste0(
                size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                '_m', num_replicates, '_', x_type, '_', error_distribution,
                '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            }
            load(paste0(dir_data_size_power, '/',
                        scenario_filename_stem, '.Rdata'))
            for (chunk_row_index in seq_along(chosen_method_names)) {
              method_index_in_results <-
                which(names(null_rejection_rates) ==
                        chosen_method_names[[chunk_row_index]])
              chunk_of_rows[chunk_row_index, chunk_column_index] <-
                null_rejection_rates[[method_index_in_results]] * 100
            }
          }
        }
        compiled_results <- rbind(compiled_results, chunk_of_rows)
      }
      compiled_results <- compiled_results[-1, ] #remove placeholder row
      rownames(compiled_results) <- row_names
      colnames(compiled_results) <- col_names
      write.csv(compiled_results,
                file = paste0(dir_tables, '/', table_filename_stem, '.csv'))
      save(list = c('compiled_results', 'caption',
                    'values_for_num_x_variables',
                    'values_for_error_corr_strength',
                    'row_group_index', 'header_row',
                    'row_group_names', 'col_group_names',
                    'row_group_size', 'col_group_size',
                    'num_row_groups', 'num_col_groups',
                    'row_names', 'col_names'),
           file = paste0(dir_tables, '/', table_filename_stem, '.Rdata'))
    }
  }
}