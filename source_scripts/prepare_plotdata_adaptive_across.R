
# Kernel Selection --------------------------------------------------------

for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {

      # file path for output plot data
      part1 <- paste0('m', num_replicates, '_', x_type, '_',
                      error_distribution, '_s', signal_strength * 100)
      if (x_type == 'snp') {
        part2 <- paste0('_sc-', signal_correlation)
      } else { part2 <- character() }
      plotdata_file <- file.path(dir_data_adaptive_across,
                                 paste0(part1, part2, '_plotdata.Rdata'))

      # Check first if plot file exists
      if (!file.exists(plotdata_file) | reprocess_existing_data) {

        # Initialize plot data frames for kernel selection
        plotdata_Y1 <- plotdata_Y2 <- plotdata_Y3 <- plotdata_Y4 <-
          data.frame(kernel = character(), signals = integer(), rho = double(),
                     samplesize = integer(), featdim = integer(),
                     value = double())
        newrow_Y1 <- newrow_Y2 <- newrow_Y3 <- newrow_Y4 <-
          data.frame(kernel = '', signals = 0L, samplesize = 0L,  rho = 0.,
                     featdim = 0L, value = 0.)

        # Initialize plot data frames for feature selection
        plotdata_phimr <-
          data.frame(datatype = character(), signals = integer(),
                     rho = double(), samplesize = integer(),
                     featdim = integer(), value = double())
        newrow_phimr <-
          data.frame(datatype = '', signals = 0L, samplesize = 0L,  rho = 0.,
                     featdim = 0L, value = 0.)

        # Populate plot data
        for (error_correlation_strength in values_for_error_corr_strength) {
          newrow_Y1$rho <- newrow_Y2$rho <- newrow_Y3$rho <-
            newrow_Y4$rho <- newrow_phimr$rho <-
            error_correlation_strength
          for (signal_density_index in seq_along(values_for_signal_density)) {
            signal_density <- values_for_signal_density[[signal_density_index]]
            n_signals <- newrow_Y1$signals <- newrow_Y2$signals <-
              newrow_Y3$signals <- newrow_Y4$signals <- newrow_phimr$signals <-
              num_signals[[signal_density_index]]
            for (n in values_for_sample_size) {
              newrow_Y1$samplesize <- newrow_Y2$samplesize <-
                newrow_Y3$samplesize <- newrow_Y4$samplesize <-
                newrow_phimr$samplesize <- n
              for (p in values_for_num_x_variables) {
                newrow_Y1$featdim <- newrow_Y2$featdim <-
                  newrow_Y3$featdim <- newrow_Y4$featdim <-
                  newrow_phimr$featdim <- p

                # File containing data for current scenario
                part1 <- paste0('m', num_replicates, '_', x_type, '_',
                                error_distribution, '_s', signal_strength * 100)
                if (x_type == 'snp') {
                  part2 <- paste0('_sc-', signal_correlation)
                } else { part2 <- character() }
                stem <- paste0(
                  part1, part2, '_', signal_density,
                  '_c', error_correlation_strength * 100, '_n', n, '_p', p)
                data_filepath <- file.path(dir_data_adaptive_across,
                                           paste0(stem, '.Rdata'))
                load(data_filepath)

                source(file.path(dir_src, 'initialize_simulation_scenario.R'))
                signal_indices <-
                  unique(c(signal_indices_shared, signal_indices_y1,
                           signal_indices_y2_only, signal_indices_y3,
                           signal_indices_y4))

                newrow_phimr$datatype <- 'density'
                # value = mean(signal density after / signal density before)
                #   (note: signal density before is constant)
                newrow_phimr$value <-
                  mean( # across data replicates
                    apply(selected_x_columns, MARGIN = 2, # to each replicate
                          FUN = function(x) { # signal density after PhiMr
                            sum(x[signal_indices]) / sum(x)})
                  ) / (n_signals / p) # divide by signal density before
                plotdata_phimr <- rbind(plotdata_phimr, newrow_phimr)

                newrow_phimr$datatype <- 'signal'
                # value = mean(# signals after phimr / # signals before)
                newrow_phimr$value <-
                  mean( # across data replicates
                    apply(selected_x_columns, MARGIN = 2, # to each replicate
                          FUN = function(x) { sum(x[signal_indices]) })
                  ) / n_signals # divide by original number of signals
                plotdata_phimr <- rbind(plotdata_phimr, newrow_phimr)

                newrow_phimr$datatype <- 'noise'
                # value = mean(# noise variables after phimr / # before)
                newrow_phimr$value <-
                  mean( # across data replicates
                    apply(selected_x_columns, MARGIN = 2, # to each replicate
                          FUN = function(x) { sum(x[-signal_indices]) })
                  ) / (p - n_signals) # divide by original # of noise variables
                plotdata_phimr <- rbind(plotdata_phimr, newrow_phimr)

                for (kernel_nametag in kernel_list) {
                  newrow_Y1$kernel <- newrow_Y2$kernel <-
                    newrow_Y3$kernel <- newrow_Y4$kernel <- kernel_nametag

                  newrow_Y1$value <-
                    mean(selected_kernels[1, ] == kernel_nametag)
                  newrow_Y2$value <-
                    mean(selected_kernels[2, ] == kernel_nametag)
                  newrow_Y3$value <-
                    mean(selected_kernels[3, ] == kernel_nametag)
                  newrow_Y4$value <-
                    mean(selected_kernels[4, ] == kernel_nametag)

                  plotdata_Y1 <- rbind(plotdata_Y1, newrow_Y1)
                  plotdata_Y2 <- rbind(plotdata_Y2, newrow_Y2)
                  plotdata_Y3 <- rbind(plotdata_Y3, newrow_Y3)
                  plotdata_Y4 <- rbind(plotdata_Y4, newrow_Y4)
                }

              }
            }
          }
        }
        plotdata_phimr$datatype <- factor(
          plotdata_phimr$datatype, levels = c('signal', 'noise', 'density'))
        plotdata_phimr$featdim <- factor(
          plotdata_phimr$featdim, levels = values_for_num_x_variables)
        plotdata_Y1$kernel <- factor(plotdata_Y1$kernel, levels = kernel_list)
        plotdata_Y2$kernel <- factor(plotdata_Y2$kernel, levels = kernel_list)
        plotdata_Y3$kernel <- factor(plotdata_Y3$kernel, levels = kernel_list)
        plotdata_Y4$kernel <- factor(plotdata_Y4$kernel, levels = kernel_list)
        plotdata_Y1$featdim <- factor(plotdata_Y1$featdim,
                                      levels = values_for_num_x_variables)
        plotdata_Y2$featdim <- factor(plotdata_Y2$featdim,
                                      levels = values_for_num_x_variables)
        plotdata_Y3$featdim <- factor(plotdata_Y3$featdim,
                                      levels = values_for_num_x_variables)
        plotdata_Y4$featdim <- factor(plotdata_Y4$featdim,
                                      levels = values_for_num_x_variables)
        plotdata_kersel <- list("Y1" = plotdata_Y1,
                                "Y2" = plotdata_Y2,
                                "Y3" = plotdata_Y3,
                                "Y4" = plotdata_Y4)
        # Save plot data
        save(plotdata_phimr, plotdata_kersel,
             file = plotdata_file)
      }
    }
  }
}
