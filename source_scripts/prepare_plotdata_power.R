
# Prepare Working Environment ---------------------------------------------

size_or_power <- 'power'
source(file.path(dir_src, 'initialize_size_power.R'))


# Load and process data ---------------------------------------------------

for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      for (p in values_for_num_x_variables) {

        # Output file path
        part1 <- paste0('m', num_replicates, '_', x_type, '_',
                        error_distribution, '_s', signal_strength * 100)
        if (x_type == 'snp') {
          part2 <- paste0('_sc-', signal_correlation)
        } else { part2 <- character() }
        plotdata_filename <- paste0(part1, part2, '_p', p, '.Rdata')
        plotdata_filepath <- file.path(dir_plotdata_power, plotdata_filename)

        # Check first whether output file exists
        if (!file.exists(plotdata_filepath) | reprocess_existing_data) {

          #### load and format data ####
          plot_data_main <- plot_data_amkat <- # initialize
            data.frame(
              method = character(),
              signals = integer(),
              sample_size = integer(),
              corr = double(),
              value = double()
            )

          new_row <-  # initialize
            data.frame(
              method = '',
              signals = 0L,
              sample_size = 0L,
              corr = 0,
              value = 0
            )

          # Populate plot data
          for (corr_ind in seq_along(values_for_error_corr_strength)) {
            error_correlation_strength <-
              values_for_error_corr_strength[[corr_ind]]

            # Record error correlation strength
            new_row$corr <- error_correlation_strength

            for (density_index in seq_along(values_for_signal_density)) {
              signal_density <- values_for_signal_density[[density_index]]

              # Record number of signals
              if (x_type == 'cts') new_row$signals <-
                  switch(signal_density, 'sparse' = 7L, 'dense' = 80L)
              if (x_type == 'snp') new_row$signals <-
                  switch(signal_density, 'sparse' = 28L, 'dense' = 122L)

              for (n in values_for_sample_size) {

                # Record sample size
                new_row$sample_size <- n

                source(file.path(dir_src, "define_scenario_filename.R"))
                load(file.path(dir_data_size_power,
                               paste0(scenario_filename_stem, '.Rdata')))

                # main plot data
                for (i in seq_along(method_labels_main)) {

                  # Record testing method
                  new_row$method <- method_labels_main[[i]]

                  # Find index of current method within loaded data
                  method_index_in_results <-
                    which(
                      names(null_rejection_rates) == chosen_methods_main[[i]])

                  # Record null rejection rate from loaded data
                  new_row$value <-
                    null_rejection_rates[[method_index_in_results]]

                  # Append current row to plot data
                  plot_data_main <- rbind(plot_data_main, new_row)
                }

                # AMKAT methods plot data
                for (i in seq_along(method_labels_amkat)) {

                  # Record testing method
                  new_row$method <- method_labels_amkat[[i]]

                  # Find index of current method within loaded data
                  method_index_in_results <-
                    which(
                      names(null_rejection_rates) == chosen_methods_amkat[[i]])

                  # Record null rejection rate from loaded data
                  new_row$value <-
                    null_rejection_rates[[method_index_in_results]]

                  # Append current row to plot data
                  plot_data_amkat <- rbind(plot_data_amkat, new_row)
                }
              }
            }
          } # Done populating plot data

          # Convert testing method to factor
          plot_data_main$method <-
            factor(plot_data_main$method, levels = method_labels_main)
          plot_data_amkat$method <-
            factor(plot_data_amkat$method, levels = method_labels_amkat)

          # Save output file
          save(plot_data_main, plot_data_amkat, file = plotdata_filepath)
        }
      }
    }
  }
}