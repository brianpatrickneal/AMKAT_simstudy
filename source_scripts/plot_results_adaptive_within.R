
# Create Plots ------------------------------------------------------------

if (x_type == 'snp') values_for_num_x_variables <- 567

for (error_distribution in values_for_error_distribution) {

  for (strength_ind in seq_along(values_for_signal_strength)) {
    signal_strength <- values_for_signal_strength[[strength_ind]]

    for (density_index in seq_along(values_for_signal_density)) {
      signal_density <- values_for_signal_density[[density_index]]

      for (corr_ind in seq_along(values_for_error_corr_strength)) {
        error_correlation_strength <- values_for_error_corr_strength[[corr_ind]]

        for (n in values_for_sample_size) {

          for (p in values_for_num_x_variables) {

            # plot file names
            pv_files <- pvaluePlotFiles()

            # Check first if all files exist
            if (!pv_files$plots_exist | overwrite_existing_plots) {

              # Load plot data
              load(pv_files$stats)
              load(pv_files$pvalues)

              #### Distribution of test stats and perm stats ####
              if (!file.exists(pv_files$plot_stats) |
                  overwrite_existing_plots) {
                distr_plots <-
                  makeStatDistrPlots(test_stats, perm_stats,
                                     perm_stats_no_filter, test_stat_no_filter,
                                     title_settings = title_statDistr(
                                       subtitle_overlay_margins =
                                         margin(t = -15, b = -15, l = 7)))
                cowplot::save_plot(
                  pv_files$plot_stats, plot = distr_plots, ncol = 2,
                  base_height = height_statDistr,
                  base_asp = asp_statDistr)
              }

              #### P-Value Distribution vs Q ####
              # one plot for each value of B
              if (!file.exists(pv_files$plot_violinQ) |
                  overwrite_existing_plots) {
                violinQ <-
                  makeViolinQPlots(
                    pvalues,
                    title_settings = title_violinQ(
                      num_replicates,
                      subtitle_overlay_margins =
                        margin(t = -20, b = -10, l = 7)))
                cowplot::save_plot(
                  pv_files$plot_violinQ, plot = violinQ, ncol = 1,
                  base_height = height_violinQ, base_asp = asp_violinQ)
              }

              #### P-Value Distribution vs B ####
              # one plot for no filter and one plot for each value of Q
              if (!file.exists(pv_files$plot_violinB) |
                  overwrite_existing_plots) {
                violinB <-
                  makeViolinBPlots(
                    pvalues,
                    title_settings = title_violinB(
                      num_replicates,
                      subtitle_overlay_margins =
                        margin(t = -20, b = -10, l = 7)))
                cowplot::save_plot(
                  pv_files$plot_violinB, plot = violinB, ncol = 1,
                  base_height = height_violinB, base_asp = asp_violinB)
              }

              #### P-Value SD ####
              if (!file.exists(pv_files$plot_sd) | overwrite_existing_plots) {
                sd_lineplots <-
                  makePVLinePlots(
                    pvalues, num_replicates,
                    title_settings =
                      title_pvline(
                        title_overlay_margins = margin(t = 7, l = 7),
                        subtitle_overlay_margins = margin(t = -15, l = 7)))
                cowplot::save_plot(
                  pv_files$plot_sd, plot = sd_lineplots, ncol = 2,
                  base_height = height_pvline, base_asp = asp_pvline)
              }

              #### Mean P-Value ####
              if (!file.exists(pv_files$plot_mean) |
                  overwrite_existing_plots) {
                mean_lineplots <-
                  makePVLinePlots(
                    pvalues, num_replicates, summary_stat = mean,
                    title_settings =
                      title_pvline(
                        joint_title = "AMKAT Mean P-value on a Fixed Data Set",
                        title_overlay_margins = margin(t = 7, l = 7),
                        subtitle_overlay_margins = margin(t = -15, l = 7)))
                cowplot::save_plot(
                  pv_files$plot_mean, plot = mean_lineplots, ncol = 2,
                  base_height = height_pvline, base_asp = asp_pvline)
              }

              #### Signal/Noise Retention by PhiMr ####
              if (!file.exists(pv_files$plot_phimr) |
                  overwrite_existing_plots) {
                cowplot::save_plot(
                  pv_files$plot_phimr,
                  plot =
                    makePhimrHistograms(
                      feature_select_rates,
                      getSignalIndices(x_type, signal_density)),
                  ncol = 2,
                  base_height = height_phimr_within,
                  base_asp = asp_phimr_within)
              }



              # ## Feature selection ##
              # signal_indices <-
              #   unique(c(signal_indices_shared, signal_indices_y1,
              #            signal_indices_y2_only, signal_indices_y3,
              #            signal_indices_y4))
              # plot_title <- paste0(
              #   'Distribution of feature filter-pass-rates on a simulated data set,\n',
              #   feature_type, ', ',
              #   num_signals[[density_index]], "-variable signal at ",
              #   100 * values_for_signal_strength[[strength_ind]], '% strength, ',
              #   'p = ', p, ', n = ', n,
              #   '\nMultivariate ', err_dist, ' ', error_corr_label[[corr_ind]])
              # pdf(file = file.path(dir_plots_adaptive_within,
              #                   paste0(scenario_filename_stem, '_features.pdf')),
              #     width = 6, height = 2.5)
              # plot_left <- makeFilterPlot(feature_select_rates[signal_indices],
              #                             'darkgreen', 'lightgreen',
              #                             'Signal variables')
              # plot_right <- makeFilterPlot(feature_select_rates[-signal_indices],
              #                              'darkred', 'indianred1',
              #                              'Noise variables')
              # grid.arrange(plot_left, plot_right, ncol = 2,
              #              top = textGrob(plot_title,
              #                             gp = gpar(fontface = 'bold')))
              # dev.off()


              # ## Kernel selection ##
              #
              # label_y1 <- paste0('Y1\n(', kernel_select_rates$no_filter[[1]], ')')
              # label_y2 <- paste0('Y2\n(', kernel_select_rates$no_filter[[2]], ')')
              # label_y3 <- paste0('Y3\n(', kernel_select_rates$no_filter[[3]], ')')
              # label_y4 <- paste0('Y4\n(', kernel_select_rates$no_filter[[4]], ')')
              # if (x_type == 'snp') {
              #   plot_data <- data.frame(
              #     ker = factor(
              #       c(rep('lin', 4), rep('quad', 4), rep('gau', 4),
              #         rep('exp', 4), rep('IBS', 4)),
              #       levels = c('lin', 'quad', 'gau', 'exp', 'IBS')),
              #     y_variable = as.factor(rep(c(label_y1, label_y2, label_y3,
              #                                  label_y4), times = 5)),
              #     value = c(kernel_select_rates$lin, kernel_select_rates$quad,
              #               kernel_select_rates$gau, kernel_select_rates$exp,
              #               kernel_select_rates$IBS))
              # }
              # if (x_type == 'cts') {
              #   plot_data <- data.frame(
              #     ker = factor(c(rep('lin', 4), rep('quad', 4),
              #                    rep('gau', 4), rep('exp', 4)),
              #                  levels = c('lin', 'quad', 'gau', 'exp')),
              #     y_variable = as.factor(rep(c(label_y1, label_y2, label_y3,
              #                                  label_y4), times = 4)),
              #     value = c(kernel_select_rates$lin, kernel_select_rates$quad,
              #               kernel_select_rates$gau, kernel_select_rates$exp))
              # }
              # pdf(file = file.path(dir_plots_adaptive_within,
              #                   paste0(scenario_filename_stem, '_kernels.pdf')),
              #     width = 4, height = 3)
              # plot <- ggplot(plot_data,
              #                aes(fill = ker, y = value, x = y_variable)) +
              #   geom_bar(position = 'dodge', stat = 'identity') +
              #   scale_fill_manual(values = color_scheme) + theme_minimal() +
              #   theme(legend.position = "bottom",
              #         legend.key.size = unit(11, "pt"),
              #         legend.text = element_text(size = 9, color = "grey30"),
              #         legend.box.spacing = unit(0, "pt"),
              #         legend.margin = margin(10, 0, 0, 0, unit = "pt"),
              #         axis.text.x = element_text(size = 10),
              #         axis.title.x = element_blank(),
              #         panel.grid.major.x = element_blank()) +
              #   labs(x = 'Response Variable', y = 'Kernel Selection Rate',
              #        fill = NULL)
              # grid.arrange(plot, ncol = 1)
              # dev.off()
            }
          }
        }
      }
    }
  }
}