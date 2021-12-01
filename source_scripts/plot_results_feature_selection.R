# --- Initialize Preliminary Objects -------------------------------------------
size_or_power <- 'power'
if (x_type == 'snp') values_for_num_x_variables <- 567
num_permutations <- 1000L
alpha <- 0.05
five_color_scheme <- c("purple4", "deepskyblue2", "green4", "darkorange1" ,
                       "magenta3")
two_color_scheme <- c('deepskyblue2', 'darkorange1')
shape_scheme <- c(15, 16, 17, 18, 0, 4, 3)
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
# Define functions to generate plots -------------------------------------------
# mean proportion of features kept
makePlotKeepRates <- function(num_signals = 0) {
  out <- ggplot(plot_data[which((plot_data$datatype %in% c('signal', 'noise')) &
                                  (plot_data$signals == num_signals)), ],
                aes(x = samplesize, y = value,
                    group = datatype, shape = datatype, color = datatype)) +
    theme_minimal() + ylim(0, 1) +
    geom_line(aes(color = datatype), size = 0.75) +
    geom_point(aes(shape = datatype, color = datatype), size = 2) +
    theme(legend.position="bottom", legend.key.size = unit(11, "pt"),
          legend.text = element_text(size = 9, color = "grey30"),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(5, 0, 0, 0, unit = "pt"),
          legend.title = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(face = 'bold')) +
    scale_color_manual(name = 'legendtitle', labels = c('signal', 'noise'),
                       values = two_color_scheme) +
    scale_shape_manual(name = 'legendtitle', labels = c('signal', 'noise'),
                       values = shape_scheme) +
    labs(title = 'Mean Proportion of Features Kept',
         x = 'sample size', y = 'proportion kept',
         subtitle = paste0(num_signals, ' signal variables before filtering'))
  return(out)
}
# relative signal density after filtering compared to before
makePlotSignalDensity <- function(num_signals = 0) {
  out<- ggplot(plot_data[which((plot_data$datatype == 'density') &
                                 (plot_data$signals == num_signals)), ],
               aes(x = samplesize, y = value,
                   group = featdim, color = featdim, shape = featdim)) +
    theme_minimal() + ylim(1, 2) +
    geom_line(aes(color = featdim), size = 0.5) +
    geom_point(aes(shape = featdim, color = featdim), size = 2) +
    theme(legend.position='bottom', legend.key.size = unit(11, "pt"),
          legend.text = element_text(size = 9, color = "grey30"),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(5, 0, 0, 0, unit = "pt"),
          legend.title = element_text(size = 9, color = "grey30"),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(face = 'bold')) +
    scale_color_manual(name = 'Feature dimension',
                       labels = values_for_num_x_variables,
                       values = five_color_scheme) +
    scale_shape_manual(name = 'Feature dimension',
                       labels = values_for_num_x_variables,
                       values = shape_scheme) +
    labs(title = 'Relative Signal Density After Filtering',
         x = 'sample size', y = 'density after / density before',
         subtitle = paste0(
           'Mean value across ', num_replicates , ' replicates\n',
           num_signals, ' signal variables before filtering'))
  return(out)
}
# --- Create Plots -------------------------------------------------------------
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (error_correlation_strength in values_for_error_corr_strength) {
      plot_filename_stem <-
        paste0('m', num_replicates, '_', x_type, '_', error_distribution, '_s',
               signal_strength * 100, '_c', error_correlation_strength * 100)
      plot_data <- data.frame(datatype = character(), signals = integer(),
                              samplesize = integer(), featdim = integer(),
                              value = double())
      new_row <- data.frame(datatype = '', signals = 0L, samplesize = 0L,
                            featdim = 0L, value = 0)
      for (index_signal_density in seq_along(values_for_signal_density)) {
        signal_density <- values_for_signal_density[[index_signal_density]]
        new_row$signals <- num_signals[[index_signal_density]]
        for (n in values_for_sample_size) {
          new_row$samplesize <- n
          p <- values_for_num_x_variables[[1]] # no effect on keeprate
          new_row$featdim <- p
          source(paste0(dir_src, '/', 'initialize_simulation_scenario.R'))
          signal_indices <-
            unique(c(signal_indices_shared, signal_indices_y1,
                     signal_indices_y2_only, signal_indices_y3,
                     signal_indices_y4))
          data_filename_stem <-
            paste0('m', num_replicates, '_', x_type, '_', error_distribution,
                   '_s', signal_strength * 100, '_', signal_density,
                   '_c', error_correlation_strength * 100, '_n', n, '_p', p)
          load(paste0(dir_data_feature_kernel_selection, '/',
                      data_filename_stem, '.Rdata'))
          new_row$datatype <- 'signal'
          new_row$value <- mean(selected_x_columns[signal_indices, ])
          plot_data <- rbind(plot_data, new_row)
          new_row$datatype <- 'noise'
          new_row$value <- mean(selected_x_columns[-signal_indices, ])
          plot_data <- rbind(plot_data, new_row)
          for (p in values_for_num_x_variables) {
            new_row$featdim <- p
            source(paste0(dir_src, '/', 'initialize_simulation_scenario.R'))
            data_filename_stem <-
              paste0('m', num_replicates, '_', x_type, '_', error_distribution,
                     '_s', signal_strength * 100, '_', signal_density,
                     '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            load(paste0(dir_data_feature_kernel_selection, '/',
                        data_filename_stem, '.Rdata'))
            signal_indices <-
              unique(c(signal_indices_shared, signal_indices_y1,
                       signal_indices_y2_only, signal_indices_y3,
                       signal_indices_y4))
            new_row$datatype <- 'density'
            # mean(signal density after / signal density before), all replicates
            new_row$value <-
              (mean(apply(selected_x_columns, MARGIN = 2,
                          function(x){sum(x[signal_indices]) / sum(x)}))) /
              (num_signals[[index_signal_density]] / p)
            plot_data <- rbind(plot_data, new_row)
          }
        }
      }
      plot_data$datatype <- factor(plot_data$datatype,
                                   levels = c('signal', 'noise', 'density'))
      plot_data$featdim <- factor(plot_data$featdim,
                                  levels = values_for_num_x_variables)

      pdf(file = paste0(dir_plots_feature_selection, '/',
                        plot_filename_stem, '.pdf'), width = 8, height = 8)
      plot_topleft <- makePlotKeepRates(num_signals[[1]])
      plot_topright <- makePlotKeepRates(num_signals[[2]])
      plot_bottomleft <- makePlotSignalDensity(num_signals[[1]])
      plot_bottomright <- makePlotSignalDensity(num_signals[[2]])
      gridExtra::grid.arrange(plot_topleft, plot_topright,
                              plot_bottomleft, plot_bottomright, ncol = 2)
      dev.off()
    }
  }
}