
# Description -------------------------------------------------------------
# Format simulated size/power results for generating LaTeX tables using
# RMarkdown, knitr and the 'kableExtra' package



# Prepare Working Environment ---------------------------------------------

# compile results for which methods?
# subset of method_names in 'source_scripts/intialize_simulation_scenario.R'
included_methods <- character()
if (size_or_power == 'power') included_methods <- c(included_methods,
                                                    'AMKAT_nofilter_pseudo')
included_methods <- c(included_methods,
                      'AMKAT_pseudo',
                      'AMKAT_avgstat_2_pseudo',
                      'AMKAT_avgstat_4_pseudo',
                      'AMKAT_avgstat_8_pseudo',
                      'AMKAT_avgstat_16_pseudo',
                      'AMKAT_avgstat_32_pseudo',
                      'AMKAT_avgstat_64_pseudo',
                      'AMKAT_avgstat_128_pseudo',
                      'OMGA_residual_Y', 'DKAT', 'DK_L')

# corresponding method labels to be used in tables/plots
method_labels <- character()
if (size_or_power == 'power') method_labels <-
  c(method_labels, 'AMKAT (no filter)')
method_labels <- c(method_labels,
                   'AMKAT-PF1',
                   'AMKAT-PF2',
                   'AMKAT-PF4',
                   'AMKAT-PF8',
                   'AMKAT-PF16',
                   'AMKAT-PF32',
                   'AMKAT-PF64',
                   'AMKAT-PF128',
                   'OMGA', 'DKAT', 'DKAT-LPK')

makeCaption <- function(size_or_power, x_type, num_replicates, num_permutations,
                        alpha,
                        signal_correlation = NULL, #only for SNP power
                        p = NULL # only for Cts power
) {
  if (x_type == 'cts') {
    feature_label <- ' using continuous features'
    if (size_or_power == 'power') feature_label <-
        paste0(feature_label, ' $(p = ', p, ')$')
  }
  if (x_type == 'snp') {
    feature_label <- ' using a simulated SNP-set ($p = 567$)'
    if (size_or_power == 'power') {
      if (signal_correlation == 'high') feature_label <-
          paste0(feature_label, 'with highly-correlated signal variables')
      if (signal_correlation == 'low') feature_label <-
          paste0(feature_label, 'with mildly-correlated signal variables')
    }
  }
  caption <- paste0(
    "Empirical ", size_or_power, " (percentage) across ", num_replicates,
    " simulated data replicates ", feature_label,
    ". All tests performed at significance level $\\alpha = ", alpha, "$. ",
    "AMKAT P-values estimated using ", num_permutations, " permutations. ",
    "AMKAT-PF$m$ denotes AMKAT with PhiMr filter using $Q=m$ test statistics for $P$-value estimation; DKAT-LPK denotes DKAT with linear phenotype kernel.")
  return(caption)
}



# Row group metadata ------------------------------------------------------

if (size_or_power == 'size') { # rows grouped by error distribution
  row_group_names <- sapply(
    values_for_error_distribution,
    function(x) {
      paste0('Multivariate ', switch(x, cauchy = "Cauchy", normal = "normal"),
             ' errors') })
}
if (size_or_power == 'power') {# rows grouped by signal density
  row_group_names <- sapply(
    values_for_signal_density,
    function(sig_density) {
      paste0(
        switch(x_type,
               cts = switch(sig_density, sparse = 7, dense = 80),
               snp = switch(sig_density, sparse = 28, dense = 122)),
        "-variable signal set") })
}
num_row_groups <- switch(size_or_power,
                         size = length(values_for_error_distribution),
                         power = length(values_for_signal_density))
row_group_size <- length(included_methods) # rows per row group
row_names <- rep(method_labels, times = num_row_groups)
# vector of row counts for each row group;
# each element is named with the name of the corresponding row group;
# used by kableExtra::packrows() to format row groups in table
row_group_index <- rep(row_group_size, times = num_row_groups)
names(row_group_index) <- row_group_names


# Column group metadata ---------------------------------------------------


if (size_or_power == 'size' & x_type == 'cts') {
  # Two column grouping levels;
  #    top header row groups columns by error correlation (rho)
  #    bottom header row groups columns by feature dimension (p)
  inner_col_group_size <- length(values_for_sample_size)
  inner_groups_per_outer_group <- length(values_for_num_x_variables)
  outer_col_group_size <- inner_col_group_size * inner_groups_per_outer_group
  num_outer_col_groups <- length(values_for_error_corr_strength)
  num_inner_col_groups <- num_outer_col_groups * inner_groups_per_outer_group
  num_columns <- num_inner_col_groups * inner_col_group_size
  col_names_one_inner_group <- paste0('$n = ', values_for_sample_size, '$')
  col_names_one_outer_group <- rep(col_names_one_inner_group,
                                   times = inner_groups_per_outer_group)
  col_names <- rep(col_names_one_outer_group, times = num_outer_col_groups)
  # vector of column counts for each column group;
  # each element is named with the name of the corresponding column group;
  # used by kableExtra::add_header_above() to format column groups in table
  inner_header_row <- rep(inner_col_group_size, num_inner_col_groups)
  names_inner_header_row_one_outer_group <-
    paste0("$p = ", values_for_num_x_variables, "$")
  names(inner_header_row) <-
    rep(names_inner_header_row_one_outer_group, num_outer_col_groups)
  header_row <- c(' ' = 1, inner_header_row)

  outer_header_row <- rep(outer_col_group_size, num_outer_col_groups)
  for (i in 1:num_outer_col_groups) {
    if (values_for_error_corr_strength[[i]] == 0) {
      names(outer_header_row)[[i]] <- 'Uncorrelated error components'
    }
    if (values_for_error_corr_strength[[i]] > 0 &
        values_for_error_corr_strength[[i]] < 0.5) {
      names(outer_header_row)[[i]] <- 'Mildly correlated error components'
    }
    if (values_for_error_corr_strength[[i]] >= 0.5) {
      names(outer_header_row)[[i]] <- 'Correlated error components'
    }
  }
  outer_header_row <- c(' ' = 1, outer_header_row)
} else {
  # One column grouping level;
  #     header row groups columns by error correlation (rho)
  outer_header_row <- NULL
  num_col_groups <- length(values_for_error_corr_strength)
  col_group_size <- length(values_for_sample_size) # cols per col group
  col_names_one_group <- paste0('$n = ', values_for_sample_size, '$')
  col_names <- rep(col_names_one_group, times = num_col_groups)
  num_columns <- num_col_groups * col_group_size
  # vector of column counts for each column group;
  # each element is named with the name of the corresponding column group;
  # used by kableExtra::add_header_above() to format column groups in table
  header_row <- rep(col_group_size, times = num_col_groups)
  for (i in 1:num_col_groups) {
    if (values_for_error_corr_strength[[i]] == 0) {
      names(header_row)[[i]] <- 'Uncorrelated error components'
    }
    if (values_for_error_corr_strength[[i]] > 0 &
        values_for_error_corr_strength[[i]] < 0.5) {
      names(header_row)[[i]] <- 'Mildly correlated error components'
    }
    if (values_for_error_corr_strength[[i]] >= 0.5) {
      names(header_row)[[i]] <- 'Correlated error components'
    }
  }
  header_row <- c(' ' = 1, header_row)
}
# Compile and export -----------------------------------------------------------

# initialize temporary storage object for iterative use
chunk_of_rows <- matrix(NA, nrow = row_group_size, ncol = num_columns)

# compile and save results separately for each set of scenarios
for (signal_strength in values_for_signal_strength) {
  for (signal_correlation in values_for_snp_signal_corr) {
    if (size_or_power == 'size') {
      outfile <-
        file.path(
          dir_tables,
          paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                 '_m', num_replicates, '_', x_type, '.Rdata'))
      if (!file.exists(outfile) | overwrite_existing_tables) {
        caption <- makeCaption(size_or_power, x_type, num_replicates,
                               num_permutations, alpha)
        # Initialize output data frame
        compiled_results <- matrix(nrow = 0, ncol = num_columns)
        # Iterate over row groups (error distribution)
        for (error_distribution in values_for_error_distribution) {
          if (x_type == 'cts') {
            # Iterate over outer column groups (error correlation)
            for (i in seq_along(values_for_error_corr_strength)) { #ith col group
              error_correlation_strength <- values_for_error_corr_strength[[i]]
              outer_column_start_index <- ((i - 1) * outer_col_group_size) + 1
              # Iterate over inner column groups (feature dimension)
              for (p_ind in seq_along(values_for_num_x_variables)) {
                p <- values_for_num_x_variables[[p_ind]]
                inner_column_start_index <-
                  outer_column_start_index +
                  ((p_ind - 1) * inner_col_group_size)
                # iterate over columns within inner column group (sample size)
                for (sample_size_ind in seq_along(values_for_sample_size)) {
                  n <- values_for_sample_size[[sample_size_ind]]
                  current_col <- inner_column_start_index + sample_size_ind - 1
                  # Load data
                  source(file.path(dir_src, 'define_scenario_filename.R'))
                  load(file.path(dir_data_size_power,
                                 paste0(scenario_filename_stem, '.Rdata')))
                  # Iterate over rows (methods) in row group
                  for (current_row in seq_along(included_methods)) {
                    # loaded data index corresponding to current method
                    method_index_in_results <-
                      which(names(null_rejection_rates) ==
                              included_methods[[current_row]])
                    # Fetch value and assign to current entry of output data frame
                    chunk_of_rows[current_row, current_col] <-
                      null_rejection_rates[[method_index_in_results]] * 100
                  }
                }
              }
            }
          }
          if (x_type == 'snp') {
            p <- 567
            # Iterate over column groups
            for (i in seq_along(values_for_error_corr_strength)) { #ith col group
              error_correlation_strength <- values_for_error_corr_strength[[i]]
              column_start_index <- ((i - 1) * col_group_size) + 1
              # iterate over columns within column group
              for (sample_size_ind in seq_along(values_for_sample_size)) {
                n <- values_for_sample_size[[sample_size_ind]]
                current_col <- column_start_index + sample_size_ind - 1
                # Load data
                source(file.path(dir_src, 'define_scenario_filename.R'))
                load(file.path(dir_data_size_power,
                               paste0(scenario_filename_stem, '.Rdata')))
                # Iterate over rows (methods) in row group
                for (current_row in seq_along(included_methods)) {
                  # loaded data index corresponding to current method
                  method_index_in_results <-
                    which(names(null_rejection_rates) ==
                            included_methods[[current_row]])
                  # Fetch value and assign to current entry of output data frame
                  chunk_of_rows[current_row, current_col] <-
                    null_rejection_rates[[method_index_in_results]] * 100
                }
              }
            }
          }
          # append compiled data for current row group to output data frame
          compiled_results <- rbind(compiled_results, chunk_of_rows)
        }
        # Save output
        rownames(compiled_results) <- row_names
        colnames(compiled_results) <- col_names
        save(list = c('compiled_results', 'caption',
                      'row_group_index', 'header_row', 'outer_header_row'),
             file = outfile)
      }
    }
    if (size_or_power == 'power') {
      error_distribution <- 'normal'
      if (x_type == 'snp') {
        p <- 567
        for (signal_correlation in values_for_snp_signal_corr) {
          outfile <-
            file.path(
              dir_tables,
              paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                     '_m', num_replicates, '_', x_type, '_sc-',
                     signal_correlation, '.Rdata'))
          if (!file.exists(outfile) | overwrite_existing_tables) {
            caption <- makeCaption(size_or_power, x_type, num_replicates,
                                   num_permutations, alpha, signal_correlation)
            # Initialize output data frame
            compiled_results <- matrix(nrow = 0, ncol = num_columns)
            # Iterate over row groups (signal density)
            for (signal_density in values_for_signal_density) {
              # Iterate over column groups
              for (i in seq_along(values_for_error_corr_strength)) { #ith col group
                error_correlation_strength <- values_for_error_corr_strength[[i]]
                column_start_index <- ((i - 1) * col_group_size) + 1
                # iterate over columns within column group
                for (sample_size_ind in seq_along(values_for_sample_size)) {
                  n <- values_for_sample_size[[sample_size_ind]]
                  current_col <- column_start_index + sample_size_ind - 1
                  # Load data
                  source(file.path(dir_src, 'define_scenario_filename.R'))
                  load(file.path(dir_data_size_power,
                                 paste0(scenario_filename_stem, '.Rdata')))
                  # Iterate over rows (methods) in row group
                  for (current_row in seq_along(included_methods)) {
                    # loaded data index corresponding to current method
                    method_index_in_results <-
                      which(names(null_rejection_rates) ==
                              included_methods[[current_row]])
                    # Fetch value and assign to current entry of output data frame
                    chunk_of_rows[current_row, current_col] <-
                      null_rejection_rates[[method_index_in_results]] * 100
                  }
                }
              }
              # append compiled data for current row group to output data frame
              compiled_results <- rbind(compiled_results, chunk_of_rows)
            }
            # Save output
            rownames(compiled_results) <- row_names
            colnames(compiled_results) <- col_names
            save(list = c('compiled_results', 'caption',
                          'row_group_index', 'header_row', 'outer_header_row'),
                 file = outfile)
          }
        }
      }
      if (x_type == 'cts') {
        for (p in values_for_num_x_variables) {
          outfile <-
            file.path(
              dir_tables,
              paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                     '_m', num_replicates, '_', x_type, '_p', p, '.Rdata'))
          if (!file.exists(outfile) | overwrite_existing_tables) {
            caption <- makeCaption(size_or_power, x_type, num_replicates,
                                   num_permutations, alpha, NULL, p)
            # Initialize output data frame
            compiled_results <- matrix(nrow = 0, ncol = num_columns)
            # Iterate over row groups (signal density)
            for (signal_density in values_for_signal_density) {
              # Iterate over column groups
              for (i in seq_along(values_for_error_corr_strength)) { #ith col group
                error_correlation_strength <- values_for_error_corr_strength[[i]]
                column_start_index <- ((i - 1) * col_group_size) + 1
                # iterate over columns within column group
                for (sample_size_ind in seq_along(values_for_sample_size)) {
                  n <- values_for_sample_size[[sample_size_ind]]
                  current_col <- column_start_index + sample_size_ind - 1
                  # Load data
                  source(file.path(dir_src, 'define_scenario_filename.R'))
                  load(file.path(dir_data_size_power,
                                 paste0(scenario_filename_stem, '.Rdata')))
                  # Iterate over rows (methods) in row group
                  for (current_row in seq_along(included_methods)) {
                    # loaded data index corresponding to current method
                    method_index_in_results <-
                      which(names(null_rejection_rates) ==
                              included_methods[[current_row]])
                    # Fetch value and assign to current entry of output data frame
                    chunk_of_rows[current_row, current_col] <-
                      null_rejection_rates[[method_index_in_results]] * 100
                  }
                }
              }
              # append compiled data for current row group to output data frame
              compiled_results <- rbind(compiled_results, chunk_of_rows)
            }
            # Save output
            rownames(compiled_results) <- row_names
            colnames(compiled_results) <- col_names
            save(list = c('compiled_results', 'caption',
                          'row_group_index', 'header_row', 'outer_header_row'),
                 file = outfile)
          }
        }
      }
    }
  }
}

