
# Feature Selection Plots -------------------------------------------------

for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      # Create filenames and load plot data
      files <- adaptiveAcrossPlotFiles()
      load(files$plotdata)

      # Plot: avg keep rate across all variables grouped by type (signal vs noise)
      if (!file.exists(files$phimrplot_retention) |
          overwrite_existing_plots) {
        cowplot::save_plot(
          filename = files$phimrplot_retention,
          plot =
            makePhimrRetentionPlots(plotdata_phimr),
          ncol = 2, base_height = height_phimr, base_asp = asp_phimr)

      }
      # Plot: avg signal density after phimr relative to before
      if (!file.exists(files$phimrplot_density) |
          overwrite_existing_plots) {
        cowplot::save_plot(
          filename = files$phimrplot_density,
          plot =
            makePhimrDensityPlots(plotdata_phimr),
          ncol = 2, base_height = height_phimr, base_asp = asp_phimr)
      }
    }
  }
}