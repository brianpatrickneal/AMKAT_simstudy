height_power2 <- function(n_rows) {
  switch(as.character(n_rows), "1" = 3.25, "2" = 7)
}
asp_power2 <- function(n_rows) {
  switch(as.character(n_rows), "1" = 0.85, "2" = 0.375)
}

# Create Plots ------------------------------------------------------------

size_or_power <- 'power'
if (x_type == 'snp') values_for_num_x_variables <- 567

for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      for (p in values_for_num_x_variables) {

        # Generate filepaths for data and plots
        plot_files <- powerPlotFiles(titles = FALSE)

        # Check first whether plot files already exist
        if (!plot_files$files_exist | overwrite_existing_plots) {

          #### load plot data ####
          load(plot_files$plotdata)

          #### Generate Plots ####

          # Main plot (line plot) #
          if (!file.exists(plot_files$main) | overwrite_existing_plots) {
            plot_main <-
              makeCompoundPowerPlot2(
                plot_data_main, y_scale_max = plot_ymax,
                theme_settings = theme_power(
                  plot_margin = margin(b = 0, l = 5)
                ))
            cowplot::save_plot(
              filename = plot_files$main, plot = plot_main, ncol = 3,
              base_height = height_power2(length(unique(plot_data_main$corr))),
              base_asp = asp_power2(length(unique(plot_data_main$corr))))
          }

          # Plots for AMKAT with different values of Q #
          if (!file.exists(plot_files$qplot) | overwrite_existing_plots) {
            plot_Qvalues <-
              makeCompoundPowerPlot2(
                plot_data_amkat, test_methods = "amkat",
                color_scheme = colors_power_amkat(),
                shape_scheme = shapes_power_amkat,
                y_scale_max = plot_ymax,
                theme_settings = theme_power(show_legend_title = TRUE,
                                             legend_title_size = rel(0.8),
                                             plot_margin = margin(b = 0, l = 5)),
                title_settings = title_power(
                  joint_title =
                    "Simulated Power: Comparison of AMKAT Variations",
                  joint_title_height =
                    powerTitleHeight(length(unique(plot_data_amkat$corr))),
                  joint_subtitle_height =
                    powerSubtitleHeight(length(unique(plot_data_amkat$corr)))))
            cowplot::save_plot(
              filename = plot_files$qplot, plot = plot_Qvalues, ncol = 3,
              base_height = height_power2(length(unique(plot_data_main$corr))),
              base_asp = asp_power2(length(unique(plot_data_main$corr))))
          }

          # BarPlots for AMKAT with different values of Q #
          if (!file.exists(plot_files$qbarplot) | overwrite_existing_plots) {
            barplot_Qvalues <-
              makeCompoundPowerBarPlot2(
                plot_data_amkat, y_scale_max = plot_ymax,
                theme_settings = theme_power(show_legend_title = TRUE,
                                             legend_title_size = rel(0.8),
                                             plot_margin = margin(b = 0, l = 5)))
            cowplot::save_plot(
              filename = plot_files$qbarplot, plot = barplot_Qvalues, ncol = 3,
              base_height = height_power2(length(unique(plot_data_main$corr))),
              base_asp = asp_power2(length(unique(plot_data_main$corr))))
          }
        }
      }
    }
  }
}