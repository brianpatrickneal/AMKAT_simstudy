
# Create Plots ------------------------------------------------------------

if (x_type == 'snp') values_for_num_x_variables <- 567

for (error_distribution in values_for_error_distribution) {

  for (strength_ind in seq_along(values_for_signal_strength)) {
    signal_strength <- values_for_signal_strength[[strength_ind]]

    for (signal_correlation in values_for_snp_signal_corr) {

      for (density_index in seq_along(values_for_signal_density)) {
        signal_density <- values_for_signal_density[[density_index]]

        for (corr_ind in seq_along(values_for_error_corr_strength)) {
          error_correlation_strength <-
            values_for_error_corr_strength[[corr_ind]]

          for (n in values_for_sample_size) {

            for (p in values_for_num_x_variables) {

              # plot file names
              pv_files <- pvaluePlotFiles2()

              # Check first if all files exist
              if (!pv_files$plots_exist | overwrite_existing_plots) {

                # Load plot data
                load(pv_files$stats)
                load(pv_files$pvalues)

                #### Distribution of test stats and perm stats ####
                if (!file.exists(pv_files$plot_stats) |
                    overwrite_existing_plots) {
                  distr_plots <-
                    makeStatDistrPlots2(
                      test_stats, perm_stats, perm_stats_no_filter,
                      test_stat_no_filter,
                      title_settings = title_statDistr(
                        subtitle_overlay_margins =
                          margin(t = -15, b = -15, l = 7)))
                  cowplot::save_plot(
                    pv_files$plot_stats, plot = distr_plots, ncol = 2,
                    base_height = 3,
                    base_width = 3.25)
                }

                #### P-Value Distribution vs Q ####
                # one plot for each value of B
                if (!file.exists(pv_files$plot_violinQ) |
                    overwrite_existing_plots) {
                  violinQ <-
                    makeViolinQPlots2(
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
                    makeViolinBPlots2(
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
                    makePVLinePlots2(
                      pvalues, num_replicates,
                      title_settings =
                        title_pvline(
                          title_overlay_margins = margin(t = 7, l = 7),
                          subtitle_overlay_margins = margin(t = -15, l = 7)))
                  cowplot::save_plot(
                    pv_files$plot_sd, plot = sd_lineplots, ncol = 2,
                    base_height = 4.5/1.15, base_width = 5/1.15)
                }

                #### Mean P-Value ####
                if (!file.exists(pv_files$plot_mean) |
                    overwrite_existing_plots) {
                  mean_lineplots <-
                    makePVLinePlots2(
                      pvalues, num_replicates, summary_stat = mean,
                      title_settings =
                        title_pvline(
                          joint_title =
                            "AMKAT Mean P-value on a Fixed Data Set",
                          title_overlay_margins = margin(t = 7, l = 7),
                          subtitle_overlay_margins = margin(t = -15, l = 7)))
                  cowplot::save_plot(
                    pv_files$plot_mean, plot = mean_lineplots, ncol = 2,
                    base_height = 4.5/1.15, base_width = 5/1.15)
                }

                #### Signal/Noise Retention by PhiMr ####
                if (!file.exists(pv_files$plot_phimr) |
                    overwrite_existing_plots) {
                  cowplot::save_plot(
                    pv_files$plot_phimr,
                    plot =
                      makePhimrHistograms2(
                        feature_select_rates,
                        getSignalIndices(x_type, signal_density)),
                    ncol = 2,
                    base_height = 4.5/1.15, base_width = 5/1.15)
                }
              }
            }
          }
        }
      }
    }
  }
}