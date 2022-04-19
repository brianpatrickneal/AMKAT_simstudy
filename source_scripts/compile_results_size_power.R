
# Description -------------------------------------------------------------

# 1. Specify a select subset of the methods for which data has been simulated
#
# 2. Import and compile selected results into data frames
# with rows indexed by method and columns indexed by sample size, and
# with rows grouped by feature dimension (p)
# and columns grouped by error correlation strength
#
# 3. Create necessary metadata to create LaTeX tables with formatted row and
# column groups using RMarkdown, knitr and the 'kableExtra' package


# --- Initialize Preliminary Objects -------------------------------------------

# for Mean P-value method, compute how many P-values were used at each specified
# number of permutations-per-P-value
# method_label_mpv10 <- paste0('AMKAT_MPv_10x', num_permutations / 10, '_pseudo')
# method_label_mpv20 <- paste0('AMKAT_MPv_20x', num_permutations / 20, '_pseudo')
# method_label_mpv50 <- paste0('AMKAT_MPv_50x', num_permutations / 50, '_pseudo')

# compile results for which methods?
# subset of method_names in 'source_scripts/intialize_simulation_scenario.R'
chosen_method_names <-
  c('AMKAT_nofilter_pseudo',
    'AMKAT_pseudo',
    'AMKAT_avgstat_2_pseudo',
    'AMKAT_avgstat_4_pseudo',
    'AMKAT_avgstat_8_pseudo',
    'AMKAT_avgstat_16_pseudo',
    'AMKAT_avgstat_32_pseudo',
    'AMKAT_avgstat_64_pseudo',
    'AMKAT_avgstat_128_pseudo',
    # method_label_mpv10, method_label_mpv20, method_label_mpv50,
    'OMGA_residual_Y', 'DKAT', 'DK_L')

# corresponding method labels to be used in tables/plots
chosen_method_labels <-
  c('AMKAT-NoFilter',
    'AMKAT-PhiMr1',
    'AMKAT-PhiMr2',
    'AMKAT-PhiMr4',
    'AMKAT-PhiMr8',
    'AMKAT-PhiMr16',
    'AMKAT-PhiMr32',
    'AMKAT-PhiMr64',
    'AMKAT-PhiMr128',
    # 'AMKAT-MP10', 'AMKAT-MP20', 'AMKAT-MP50',
    'OMGA', 'DKAT', 'DKAT-LPK')

# force default values for irrelevant scenario parameters
if (size_or_power == 'size') {
  values_for_signal_strength <- NA
  values_for_signal_density <- NA
}
if (x_type == 'snp') values_for_num_x_variables <- 567

# Number of signals corresponding to each signal density value to be included
if (size_or_power == 'power') {
  num_signals <- rep(0L, length(values_for_signal_density))
  for (i in seq_along(values_for_signal_density)) {
    if (x_type == 'cts') {
      num_signals[[i]] <- switch(values_for_signal_density[[i]],
                                 'sparse' = 7L, 'dense' = 80L)
    }
    if (x_type == 'snp') {
      num_signals[[i]] <- switch(values_for_signal_density[[i]],
                                 'sparse' = 28L, 'dense' = 123L)
    }
  }
}

# row group metadata (rows grouped by feature dimension p)
num_row_groups <- length(values_for_num_x_variables)
row_group_size <- length(chosen_method_names) # rows per row group
row_group_names <- paste0('$p = ', values_for_num_x_variables, '$')
row_names <- rep(chosen_method_labels, num_row_groups)

# column group metadata (columns grouped by error correlation)
num_col_groups <- length(values_for_error_corr_strength)
col_group_size <- length(values_for_sample_size) # cols per col group
col_group_names <- paste0('$n = ', values_for_sample_size, '$')
col_names <- rep(col_group_names, num_col_groups)

# vector of row counts for each row group;
# each element is named with the name of the corresponding row group;
# used by kableExtra::packrows() to format row groups in table
row_group_index <- double() # init
for (i in 1:num_row_groups) {
  new_entry <- row_group_size # row count for group
  names(new_entry) <- paste0('Feature dimension ' ,
                             values_for_num_x_variables[[i]]) # row group name
  row_group_index <- c(row_group_index, new_entry)
}

# vector of column counts for each column group;
# each element is named with the name of the corresponding column group;
# used by kableExtra::add_header_above() to format column groups in table
header_row <- c(' ' = 1) # init
for (i in 1:num_col_groups) {
  new_entry <- col_group_size
  if (values_for_error_corr_strength[[i]] == 0) {
    names(new_entry) <- 'Independent errors'
  }
  if (values_for_error_corr_strength[[i]] > 0 &
      values_for_error_corr_strength[[i]] < 0.5) {
    names(new_entry) <- 'Mildly correlated errors'
  }
  if (values_for_error_corr_strength[[i]] >= 0.5) {
    names(new_entry) <- 'Correlated errors'
  }
  header_row <- c(header_row, new_entry)
}

# text elements for captions and labels
if (x_type == 'cts') xtype <- 'continuous'
if (x_type == 'snp') xtype <- 'discrete'

# Compile and export -----------------------------------------------------------

# initialize temporary storage object for iterative use
chunk_of_rows <-
  matrix(NA, nrow = row_group_size, ncol = (num_col_groups * col_group_size))

# compile and save results separately for each set of scenarios
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
      output_file_data <-
        file.path(dir_tables, paste0(table_filename_stem, '.Rdata'))
      output_file_csv <-
        file.path(dir_tables, paste0(table_filename_stem, '.csv'))
      all_files_exist <-
        file.exists(output_file_data) & file.exists(output_file_csv)

      # Check first whether files already exist
      if (!all_files_exist | overwrite_existing_tables) {

        # Initialize output data frame
        compiled_results <- rep(NA, length(values_for_sample_size) *
                                  length(values_for_error_corr_strength))

        # Iterate over row groups
        for (p in values_for_num_x_variables) {

          # Iterate over column groups
          for (i in seq_along(values_for_error_corr_strength)) { #ith col group
            error_correlation_strength <- values_for_error_corr_strength[[i]]

            # column number for start of current column group
            column_start_index <- (i - 1) * length(values_for_sample_size) + 1

            # iterate over columns within column group
            for (sample_size_index in seq_along(values_for_sample_size)) {
              n <- values_for_sample_size[[sample_size_index]]

              # current column number
              chunk_column_index <- column_start_index + (sample_size_index - 1)

              # import simulated results
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
              load(file.path(dir_data_size_power,
                             paste0(scenario_filename_stem, '.Rdata')))

              # Iterate over rows in row group
              for (chunk_row_index in seq_along(chosen_method_names)) {

                # Find location of corresponding value in imported results
                method_index_in_results <-
                  which(names(null_rejection_rates) ==
                          chosen_method_names[[chunk_row_index]])

                # Fetch value and assign to current entry of output data frame
                chunk_of_rows[chunk_row_index, chunk_column_index] <-
                  null_rejection_rates[[method_index_in_results]] * 100
              }
            }
          }

          # append compiled data for current row group to output data frame
          compiled_results <- rbind(compiled_results, chunk_of_rows)
        }

        # Save output
        compiled_results <- compiled_results[-1, ] #remove placeholder row
        rownames(compiled_results) <- row_names
        colnames(compiled_results) <- col_names
        write.csv(compiled_results,
                  file = output_file_csv)
        save(list = c('compiled_results', 'caption',
                      'values_for_num_x_variables',
                      'values_for_error_corr_strength',
                      'row_group_index', 'header_row',
                      'row_group_names', 'col_group_names',
                      'row_group_size', 'col_group_size',
                      'num_row_groups', 'num_col_groups',
                      'row_names', 'col_names'),
             file = output_file_data)
      }
    }
  }
}