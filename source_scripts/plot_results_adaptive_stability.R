# --- Initialize Preliminary Objects -------------------------------------------
if (size_or_power == 'size') values_for_signal_strength <- 1
if (size_or_power == 'size') values_for_signal_density <- 'sparse'
if (x_type == 'snp') values_for_num_x_variables <- 567
num_permutations <- 1000L
method_labels <-
  c(paste0('AMKAT\nwithout filter\n', num_permutations, ' perms.'),
    paste0('AMKAT\nwith filter\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n10 obs. stats\n', num_permutations, ' perms.'),
    # paste0('AMKAT-MOSt\n20 obs. stats\n', num_permutations, ' perms.'),
    # paste0('AMKAT-MOSt\n50 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n100 obs. stats\n', num_permutations, ' perms.'),
    'AMKAT-MPv\n10 tests\n@100 perms.',
    # 'AMKAT-MPv\n20 tests\n@50 perms.', 'AMKAT-MPv\n50 tests\n@20 perms.',
    'AMKAT-MPv\n100 tests\n@10 perms.')
method_labels_avgstat <-
  c(paste0('AMKAT\nwith filter\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n10 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n20 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n50 obs. stats\n', num_permutations, ' perms.'),
    paste0('AMKAT-MOSt\n100 obs. stats\n', num_permutations, ' perms.'))
color_scheme <- c("purple4", "green4" , "darkorange1" , "deepskyblue2",
                  "magenta3" )

# Define functions to generate plots -------------------------------------------
makeStatDistrPlot <- function(plot_data, plot_title, y_min, y_max,
                              line_value, line_label, line_label_position) {
  out <- ggplot(plot_data,
                aes(x = group, y = value, fill = group)) +
    geom_violin(width = 1) +
    geom_boxplot(width = 0.05, color = "gray60", alpha = 0.5,
                 outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE) +
    geom_hline(yintercept = line_value, color = 'purple') +
    annotate('text', label = line_label, x = line_label_position,
             y = line_value, size = 3, color = 'purple') +
    labs(subtitle = plot_title,
         y = 'Test Statistic Value') +
    theme_minimal() + scale_y_continuous(limits = c(y_min, y_max)) +
    theme(legend.position = "none", axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.subtitle = element_text(face = 'bold'))
  return(out)
}

preparePValuePlotData <-
  function(data, method_labels, include_rejection = FALSE) {
    plot_data <- tidyr::gather(data, key = 'group', value = 'p_value',
                               factor_key = TRUE)
    std_dev_vec <- list('group' = method_labels,
                        'std_dev' = apply(data, MARGIN = 2,
                                          function(x){round(sd(x), 4)}))
    plot_data <- dplyr::left_join(plot_data, std_dev_vec, copy = TRUE,
                                  by = 'group')
    if (include_rejection) {
      reject_rates <-
        list('group' = method_labels,
             'null_rejection_rate' = apply(data, MARGIN = 2,
                                           function(x){mean(x < alpha)}))
      plot_data <- dplyr::left_join(plot_data, reject_rates, copy = TRUE,
                                   by = 'group')
      group_labels <- paste0(reject_rates$group, '\n', size_or_power , ' = ',
                             as.character(reject_rates$null_rejection_rate),
                             '\n sd = ', std_dev_vec$std_dev)
      plot_data <- dplyr::mutate(
        plot_data, group_label =
          factor(paste0(group, '\n', size_or_power , ' = ',
                        as.character(null_rejection_rate), '\n sd = ', std_dev),
                 levels = group_labels))
    } else {
      group_labels <- paste0(std_dev_vec$group, '\n sd = ', std_dev_vec$std_dev)
      plot_data <- dplyr::mutate(
        plot_data, group_label = factor(paste0(group, '\n sd = ', std_dev),
                                        levels = group_labels))
    }
    return(plot_data)
  }

makePValuePlot <- function(plot_data, include_rejection = FALSE,
                           line_labels_x_coord = 0.55) {

  plot <- ggplot(plot_data, aes(x = group_label, y = p_value, fill = group)) +
    geom_violin(width = 1) +
    geom_boxplot(width = 0.05, color = "gray60", alpha = 0.5,
                 outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE) + theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(size = 11),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    ylab('P-value')
  if (include_rejection) {
    plot <- plot +
      geom_hline(yintercept = alpha, color = 'red') +
      annotate('text', label = 'alpha\n', x = line_labels_x_coord, y = alpha,
               size = 3, color = 'red')
    if (pvals_comparison_methods$OMGA < alpha) {
      plot <- plot +
        geom_hline(yintercept = pvals_comparison_methods$OMGA,
                   color = 'blue') +
        annotate('text', label = 'OMGA\n', x = line_labels_x_coord,
                 y = pvals_comparison_methods$OMGA, size = 3,
                 color = 'blue')
    }
    if (pvals_comparison_methods$DKAT < alpha) {
      plot <- plot +
        geom_hline(yintercept = pvals_comparison_methods$DKAT,
                   color = 'darkgreen') +
        annotate('text', label = 'DKAT\n', x = line_labels_x_coord,
                 y = pvals_comparison_methods$DKAT, size = 3,
                 color = 'darkgreen')
    }
    if (pvals_comparison_methods$DKAT_LinY < alpha) {
      plot <- plot +
        geom_hline(yintercept = pvals_comparison_methods$DKAT_LinY,
                   color = 'purple') +
        annotate('text', label = 'DKAT-LinY\n', x = line_labels_x_coord,
                 y = pvals_comparison_methods$DKAT_LinY, size = 3,
                 color = 'purple')
    }
  }
  return(plot)

}
makeFilterPlot <- function(data, bar_border_color, bar_fill_color) {
  plot <- ggplot(data.frame(kept_or_not = data), aes(x = kept_or_not)) +
    geom_histogram(color = bar_border_color, fill = bar_fill_color,
                   binwidth = 0.05) +
    theme_minimal() + xlim(-0.05, 1.05) +
    theme(panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(face = 'bold')) +
    labs(x = 'Frequency of retention', y = 'Number of Variables',
         subtitle = 'Signal Variables')
  return(plot)
}
# --- Create Plots -------------------------------------------------------------
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_density in values_for_signal_density) {
      for (error_correlation_strength in values_for_error_corr_strength) {
        for (n in values_for_sample_size) {
          for (p in values_for_num_x_variables) {
            source(paste0(dir_src, '/', 'initialize_simulation_scenario.R'))
            signal_indices <-
              unique(c(signal_indices_shared, signal_indices_y1,
                       signal_indices_y2_only, signal_indices_y3,
                       signal_indices_y4))
            if (size_or_power == 'power') {
              plot_filename_stem <- paste0(
                size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                '_m', num_replicates, '_', x_type, '_', error_distribution,
                '_s', signal_strength * 100, '_', signal_density,
                '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            }
            if (size_or_power == 'size') {
              plot_filename_stem <- paste0(
                size_or_power, '_a', alpha * 1000, '_b', num_permutations,
                '_m', num_replicates, '_', x_type, '_', error_distribution,
                '_c', error_correlation_strength * 100, '_n', n, '_p', p)
            }
            load(paste0(dir_data_adaptive_stability, '/',
                        plot_filename_stem, '.Rdata'))

            # Distribution of test stats and perm stats
            minval <- min(observed_statistics, permutation_stats,
                          permutation_stats_no_filter, test_stat_no_filter)
            maxval <- max(observed_statistics, permutation_stats,
                          permutation_stats_no_filter, test_stat_no_filter)
            mean_observed_stat <- mean(observed_statistics)

            observed_stats_long <- rep(NA, times = length(permutation_stats))
            observed_stats_long[1:length(observed_statistics)] <-
              as.vector(observed_statistics)
            observed_stats_no_filter_long <-
              rep(test_stat_no_filter, times = length(permutation_stats))

            data_stats_no_filter <-
              data.frame(cbind(observed_stats_no_filter_long,
                               as.vector(permutation_stats_no_filter)))
            data_stats <- data.frame(cbind(observed_stats_long,
                                           as.vector(permutation_stats)))

            colnames(data_stats_no_filter) <-
              c('Observed statistic',
                paste0('Permutation statistics\n(',
                       num_replicates * num_permutations, ' observations)'))
            colnames(data_stats) <-
              c(paste0('Observed statistics\n(',
                       num_replicates * 100, ' observations)'),
                paste0('Permutation statistics\n(',
                       num_replicates * num_permutations, ' observations)'))

            plot_data_stats_no_filter <-
              gather(data_stats_no_filter, key = 'group', value = 'value',
                     factor_key = TRUE) #long format
            plot_data_stats <-
              gather(data_stats, key = 'group', value = 'value',
                     factor_key = TRUE) #long format

            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_stats.pdf'),
                width = 8, height = 5)
            plot_left <-
              makeStatDistrPlot(
                plot_data_stats_no_filter, 'Without Feature Selection',
                minval, maxval, test_stat_no_filter, 'observed\nvalue', 1)
            plot_right <-
              makeStatDistrPlot(
                plot_data_stats, 'With Feature Selection', minval, maxval,
                mean_observed_stat, 'mean\nobserved\nvalue\n', 1.5)
            grid.arrange(plot_left, plot_right, ncol = 2)
            dev.off()

            # P-values, AMKAT mean stat method, grouped by # of stats
            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_pvalues_most.pdf'),
                width = 9, height = 4.5)
            plot_data <-
              preparePValuePlotData(pvalues_most, method_labels_avgstat,
                                    include_rejection = FALSE)
            plot <- makePValuePlot(plot_data, include_rejection = FALSE)
            grid.arrange(plot, ncol = 1)
            dev.off()

            # P-values, all AMKAT methods (pvalues adjusted with pseudocount)
            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_pvalues_pseudo.pdf'),
                width = 9, height = 4.5)
            plot_data <-
              preparePValuePlotData(pvalues_pseudo, method_labels,
                                    include_rejection = TRUE)
            plot <- makePValuePlot(plot_data, include_rejection = TRUE)
            grid.arrange(plot, ncol = 1)
            dev.off()

            # P-values, all AMKAT methods (floor applied to pvalues)
            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_pvalues_floor.pdf'),
                width = 9, height = 4.5)
            plot_data <-
              preparePValuePlotData(pvalues_floor, method_labels,
                                    include_rejection = TRUE)
            plot <- makePValuePlot(plot_data, include_rejection = TRUE)
            grid.arrange(plot, ncol = 1)
            dev.off()

            # Feature selection
            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_features.pdf'),
                width = 6, height = 2.5)
            plot_left <- makeFilterPlot(feature_select_rates[signal_indices],
                                        'darkgreen', 'lightgreen')
            plot_right <- makeFilterPlot(feature_select_rates[-signal_indices],
                                         'darkred', 'indianred1')
            grid.arrange(plot_left, plot_right, ncol = 2)
            dev.off()

            # Kernel selection
            label_y1 <- paste0('Y1\n(', kernel_select_rates$no_filter[[1]], ')')
            label_y2 <- paste0('Y2\n(', kernel_select_rates$no_filter[[2]], ')')
            label_y3 <- paste0('Y3\n(', kernel_select_rates$no_filter[[3]], ')')
            label_y4 <- paste0('Y4\n(', kernel_select_rates$no_filter[[4]], ')')
            if (x_type == 'snp') {
              plot_data <- data.frame(
                ker = factor(
                  c(rep('lin', 4), rep('quad', 4), rep('gau', 4),
                    rep('exp', 4), rep('IBS', 4)),
                  levels = c('lin', 'quad', 'gau', 'exp', 'IBS')),
                y_variable = as.factor(rep(c(label_y1, label_y2, label_y3,
                                             label_y4), times = 5)),
                value = c(kernel_select_rates$lin, kernel_select_rates$quad,
                          kernel_select_rates$gau, kernel_select_rates$exp,
                          kernel_select_rates$IBS))
            }
            if (x_type == 'cts') {
              plot_data <- data.frame(
                ker = factor(c(rep('lin', 4), rep('quad', 4),
                               rep('gau', 4), rep('exp', 4)),
                             levels = c('lin', 'quad', 'gau', 'exp')),
                y_variable = as.factor(rep(c(label_y1, label_y2, label_y3,
                                             label_y4), times = 4)),
                value = c(kernel_select_rates$lin, kernel_select_rates$quad,
                          kernel_select_rates$gau, kernel_select_rates$exp))
            }
            pdf(file = paste0(dir_plots_adaptive_stability, '/',
                              plot_filename_stem, '_kernels.pdf'),
                width = 4, height = 3)
            plot <- ggplot(plot_data,
                           aes(fill = ker, y = value, x = y_variable)) +
              geom_bar(position = 'dodge', stat = 'identity') +
              scale_fill_manual(values = color_scheme) + theme_minimal() +
              theme(legend.position = "bottom",
                    legend.key.size = unit(11, "pt"),
                    legend.text = element_text(size = 9, color = "grey30"),
                    legend.box.spacing = unit(0, "pt"),
                    legend.margin = margin(10, 0, 0, 0, unit = "pt"),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_blank(),
                    panel.grid.major.x = element_blank()) +
              labs(x = 'Response Variable', y = 'Kernel Selection Rate',
                   fill = NULL)
            grid.arrange(plot, ncol = 1)
            dev.off()
          }
        }
      }
    }
  }
}