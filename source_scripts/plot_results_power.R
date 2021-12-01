# --- Initialize Preliminary Objects -------------------------------------------
num_tests_at_10_perms_each <- num_permutations / 10
method_label_mpv10 <- paste0('AMKAT_MPv_10x', num_tests_at_10_perms_each)
size_or_power <- 'power'
if (x_type == 'snp') values_for_num_x_variables <- 567
chosen_methods_main <-
  c('AMKAT_nofilter_floor', 'AMKAT_floor', 'AMKAT_avgstat_128_floor',
    paste0(method_label_mpv10, '_floor'), 'OMGA_residual_Y', 'DKAT', 'DK_L')
method_labels_main <-
  c('AMKAT-NoFi', 'AMKAT', 'AMKAT-MOSt', 'AMKAT-MPv', 'OMGA', 'DKAT', 'DK-LinY')
chosen_methods_avgstat <-
  c('AMKAT_avgstat_2_floor', 'AMKAT_avgstat_4_floor', 'AMKAT_avgstat_8_floor',
    'AMKAT_avgstat_16_floor', 'AMKAT_avgstat_32_floor',
    'AMKAT_avgstat_64_floor', 'AMKAT_avgstat_128_floor')
method_labels_avgstat <- 2 ^ c(1, 2, 3, 4, 5, 6, 7)
color_scheme <- c("magenta2", "purple3", "green4", "green3",
                  "darkorange1", "blue", "deepskyblue2")
shape_scheme <- c(18, 17, 15, 16, 0, 4, 3)
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
error_corr_label <- rep('', length(values_for_error_corr_strength))
for (i in seq_along(values_for_error_corr_strength)) {
  if (values_for_error_corr_strength[[i]] == 0) {
    error_corr_label[[i]] <- 'independent'
  } else if (values_for_error_corr_strength[[i]] >= 0.5) {
    error_corr_label[[i]] <- 'correlated'
  } else {
    error_corr_label[[i]] <- 'mildly correlated'
  }
}
# Define functions to generate plots -------------------------------------------
makeLinePlot <- function(plot_data, method_labels, plot_title = 'Title',
                         legend_title = 'Method', show_legend_title = FALSE,
                         p, row_index, col_index) {
  out <-
    ggplot2::ggplot(plot_data[which(
      (plot_data$corr == values_for_error_corr_strength[[row_index]]) &
        (plot_data$signals == num_signals[[col_index]])), ],
      aes(x = sample_size, y = value,
          group = method, color = method, shape = method)) +
    theme_minimal() + ylim(0, 1) +
    geom_line(aes(color = method), size = 0.5) +
    geom_point(aes(shape = method, color = method), size = 2) +
    theme(legend.position = 'bottom', legend.key.size = unit(11, "pt"),
          legend.text = element_text(size = 9, color = "grey30"),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(5, 0, 0, 0, unit = "pt"),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(face = 'bold')) +
    scale_color_manual(name = legend_title, labels = method_labels,
                       values = color_scheme) +
    scale_shape_manual(name = legend_title, labels = method_labels,
                       values = shape_scheme) +
    labs(title = plot_title, x = 'sample size', y = 'power',
         subtitle = paste0(num_signals[[col_index]] ,' signals, ',
                           error_corr_label[[row_index]], ' errors'))
  if (show_legend_title) {
    out <- out + theme(legend.title = element_text(size = 9, color = "grey30"))
  } else {
    out <- out + theme(legend.title = element_blank())
  }
  return(out)
}
makeBarPlot <- function(plot_data, method_labels, plot_title = 'Title',
                        legend_title = 'Method', show_legend_title = FALSE,
                        p, row_index, col_index) {
  out <-
    ggplot2::ggplot(plot_data[which(
      (plot_data$corr == values_for_error_corr_strength[[row_index]]) &
        (plot_data$signals == num_signals[[col_index]])), ],
      aes(x = sample_size, y = value, group = method, fill = method)) +
    theme_minimal() + ylim(0, 1) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme(legend.position = 'bottom', legend.key.size = unit(11, "pt"),
          legend.text = element_text(size = 9, color = "grey30"),
          legend.box.spacing = unit(0, "pt"),
          legend.margin = margin(5, 0, 0, 0, unit = "pt"),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    scale_fill_manual(
      name = legend_title, labels = method_labels, values = color_scheme) +
    labs(title = plot_title, x = 'sample size', y = 'power',
         subtitle = paste0(num_signals[[col_index]] ,' signals, ',
                           error_corr_label[[row_index]], ' errors'))
  if (show_legend_title) {
    out <- out + theme(legend.title = element_text(size = 9, color = "grey30"))
  } else {
    out <- out + theme(legend.title = element_blank())
  }
  return(out)
}
# --- Create Plots -------------------------------------------------------------
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (p in values_for_num_x_variables) {
      plot_filename_stem <-
        paste0('m', num_replicates, '_', x_type, '_', error_distribution,
               '_s', signal_strength * 100, '_p', p)
      plot_data_main <- plot_data_avgstat <-
        data.frame(method = character(), signals = integer(),
                   sample_size = integer(), corr = double(), value = double())
      new_row <- data.frame(method = '', signals = 0L, sample_size = 0L,
                            corr = 0, value = 0)
      for (corrind in seq_along(values_for_error_corr_strength)) {
        error_correlation_strength <-
          values_for_error_corr_strength[[corrind]]
        new_row$corr <- error_correlation_strength
        for (signal_density_index in seq_along(values_for_signal_density)) {
          signal_density <- values_for_signal_density[[signal_density_index]]
          new_row$signals <- num_signals[[signal_density_index]]
          for (n in values_for_sample_size) {
            new_row$sample_size <- n
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
            for (i in seq_along(method_labels_main)) {
              new_row$method <- method_labels_main[[i]]
              method_index_in_results <-
                which(names(null_rejection_rates) == chosen_methods_main[[i]])
              new_row$value <- null_rejection_rates[[method_index_in_results]]
              plot_data_main <- rbind(plot_data_main, new_row)
            }
            for (i in seq_along(method_labels_avgstat)) {
              new_row$method <- method_labels_avgstat[[i]]
              method_index_in_results <-
                which(names(null_rejection_rates) ==
                        chosen_methods_avgstat[[i]])
              new_row$value <- null_rejection_rates[[method_index_in_results]]
              plot_data_avgstat <- rbind(plot_data_avgstat, new_row)
            }
          }
        }
      }
      plot_data_main$method <-
        factor(plot_data_main$method, levels = method_labels_main)
      plot_data_avgstat$method <-
        factor(plot_data_avgstat$method, levels = method_labels_avgstat)

      pdf(file = paste0(dir_plots_power, '/', plot_filename_stem, '_line.pdf'),
          width = 8, height = 8)
      plot_upper_left <- makeLinePlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE, p, 1, 1)
      plot_upper_right <- makeLinePlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 1, 2)
      plot_lower_left <- makeLinePlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 2, 1)
      plot_lower_right <- makeLinePlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 2, 2)
      gridExtra::grid.arrange(plot_upper_left, plot_upper_right,
                              plot_lower_left, plot_lower_right, ncol = 2)
      dev.off()

      pdf(file = paste0(dir_plots_power, '/', plot_filename_stem, '_bar.pdf'),
          width = 8, height = 6)
      plot_upper_left <- makeBarPlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 1, 1)
      plot_upper_right <- makeBarPlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 1, 2)
      plot_lower_left <- makeBarPlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 2, 1)
      plot_lower_right <- makeBarPlot(
        plot_data_main, method_labels_main,
        plot_title = paste0('Empirical Power (p = ', p, ')'),
        legend_title = 'Method', show_legend_title = FALSE,p, 2, 2)
      gridExtra::grid.arrange(plot_upper_left, plot_upper_right,
                              plot_lower_left, plot_lower_right, ncol = 2)
      dev.off()

      pdf(file = paste0(dir_plots_power, '/', plot_filename_stem,
                        '_most_line.pdf'),
          width = 8, height = 8)
      plot_upper_left <- makeLinePlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 1, 1)
      plot_upper_right <- makeLinePlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 1, 2)
      plot_lower_left <- makeLinePlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 2, 1)
      plot_lower_right <- makeLinePlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 2, 2)
      gridExtra::grid.arrange(plot_upper_left, plot_upper_right,
                              plot_lower_left, plot_lower_right, ncol = 2)
      dev.off()
      pdf(file = paste0(dir_plots_power, '/', plot_filename_stem,
                        '_most_bar.pdf'),
          width = 8, height = 6)
      plot_upper_left <- makeBarPlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 1, 1)
      plot_upper_right <- makeBarPlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 1, 2)
      plot_lower_left <- makeBarPlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 2, 1)
      plot_lower_right <- makeBarPlot(
        plot_data_avgstat, method_labels_avgstat,
        plot_title = paste0('AMKAT-MOSt Power (p = ', p, ')'),
        legend_title = 'obs. stats', show_legend_title = TRUE, p, 2, 2)
      gridExtra::grid.arrange(plot_upper_left, plot_upper_right,
                              plot_lower_left, plot_lower_right,
                              ncol = 2)
      dev.off()
    }
  }
}