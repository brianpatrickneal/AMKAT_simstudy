# --- Initialize Preliminary Objects -------------------------------------------
size_or_power <- 'power'
if (x_type == 'snp') values_for_num_x_variables <- 567
num_permutations <- 1000L
alpha <- 0.05
y_col_labels <- c('Y1', 'Y2', 'Y3', 'Y4')
if (x_type == 'cts') kernel_list <- c('lin', 'quad', 'gau', 'exp')
if (x_type == 'snp') kernel_list <- c('lin', 'quad', 'gau', 'exp', 'IBS')
five_color_scheme <- c("purple4", "deepskyblue2", "green4", "darkorange1" ,
                       "magenta3")
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
makeKernelPlot <- function(num_signals = 0, featdim = 0, y_col_index) {
  out <- ggplot(plot_data[which((plot_data$featdim == featdim) &
                                  (plot_data$signals == num_signals)), ],
                aes(x = samplesize, y = value,
                    group = kernel, color = kernel, shape = kernel)) +
    theme_minimal() + ylim(0, 1) +
    geom_line(aes(color = kernel), size = 0.5) +
    geom_point(aes(shape = kernel, color = kernel), size = 2) +
    theme(legend.position = 'bottom', legend.key.size = unit(11, "pt"),
          legend.text = element_text(size = 9, color = "grey30"),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(5, 0, 0, 0, unit = "pt"),
          legend.title = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(face = 'bold')) +
    scale_color_manual(name = 'Kernel', labels = kernel_list,
                       values = five_color_scheme) +
    scale_shape_manual(name = 'Kernel', labels = kernel_list,
                       values = shape_scheme) +
    labs(title = paste0('Kernel Selection Rates for ',
                        y_col_labels[[y_col_index]]),
         x = 'sample size', y = 'relative frequency of selection',
         subtitle = paste0('p = ', featdim, ' with ',
                           num_signals,' signal variables' ))
  return(out)
}
# --- Create Plots -------------------------------------------------------------
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (error_correlation_strength in values_for_error_corr_strength) {
      for (y_col_index in seq_along(y_col_labels)) {
        plot_filename_stem <-
          paste0('m', num_replicates, '_', x_type, '_', error_distribution,
                 '_s', signal_strength * 100, '_c',
                 error_correlation_strength * 100, y_col_labels[[y_col_index]])
        plot_data <- data.frame(kernel = character(), signals = integer(),
                                samplesize = integer(), featdim = integer(),
                                value = double())
        new_row <- data.frame(kernel = '', signals = 0L, samplesize = 0L,
                              featdim = 0L, value = 0)
        for (signal_density_index in seq_along(values_for_signal_density)) {
          signal_density <- values_for_signal_density[[signal_density_index]]
          new_row$signals <- num_signals[[signal_density_index]]
          for (n in values_for_sample_size) {
            new_row$samplesize <- n
            for (p in values_for_num_x_variables) {
              new_row$featdim <- p
              data_filename_stem <- paste0(
                'm', num_replicates, '_', x_type, '_', error_distribution,
                '_s', signal_strength * 100, '_', signal_density,
                '_c', error_correlation_strength * 100, '_n', n, '_p', p)
              load(paste0(dir_data_feature_kernel_selection, '/',
                          data_filename_stem, '.Rdata'))
              for (kernel_nametag in kernel_list) {
                new_row$kernel <- kernel_nametag
                new_row$value <-
                  mean(selected_kernels[y_col_index, ] == kernel_nametag)
                plot_data <- rbind(plot_data, new_row)
              }
            }
          }
        }
        plot_data$kernel <- factor(plot_data$kernel, levels = kernel_list)
        plot_data$featdim <- factor(plot_data$featdim,
                                    levels = values_for_num_x_variables)
        if (x_type == 'cts') {
          pdf(file = paste0(dir_plots_kernel_selection, '/',
                            plot_filename_stem, '.pdf'), width = 8, height = 8)
          plot_topleft <-
            makeKernelPlot(num_signals[[1]],
                           featdim = values_for_num_x_variables[[1]],
                           y_col_index)
          plot_topright <-
            makeKernelPlot(num_signals[[2]],
                           featdim = values_for_num_x_variables[[1]],
                           y_col_index)
          plot_bottomleft <-
            makeKernelPlot(num_signals[[1]],
                           featdim = values_for_num_x_variables[[4]],
                           y_col_index)
          plot_bottomright <-
            makeKernelPlot(num_signals[[2]],
                           featdim = values_for_num_x_variables[[4]],
                           y_col_index)
          gridExtra::grid.arrange(plot_topleft, plot_topright,
                                  plot_bottomleft, plot_bottomright, ncol = 2)
          dev.off()
        }
        if (x_type == 'snp') {
          pdf(file = paste0(dir_plots_kernel_selection, '/',
                            plot_filename_stem, '.pdf'), width = 8, height = 4)
          plot_left <-
            makeKernelPlot(num_signals[[1]],
                           featdim = values_for_num_x_variables[[1]],
                           y_col_index)
          plot_right <-
            makeKernelPlot(num_signals[[2]],
                           featdim = values_for_num_x_variables[[1]],
                           y_col_index)
          gridExtra::grid.arrange(plot_left, plot_right, ncol = 2)
          dev.off()
        }
      }
    }
  }
}