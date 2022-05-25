
# Kernel Selection Plots --------------------------------------------------

for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      # Create filenames and load plot data
      files <- adaptiveAcrossPlotFiles()
      load(files$plotdata)
      if (x_type == "cts") {
        for (rho_ind in seq_along(values_for_error_corr_strength)) {
          rho <- values_for_error_corr_strength[[rho_ind]]
          for (y_ind in seq_along(y_col_labels)) {
            # Check first if plot file exists
            if (!file.exists(files$kerplots[y_ind, rho_ind]) |
                overwrite_existing_plots) {
              plot_kersel <-
                makeKerSelPlots(
                  plotdata_kersel[[y_ind]], x_type, rho,
                  title_settings =
                    title_kersel(joint_title = makeKerSelTitle(y_ind),
                                 joint_subtitle =
                                   makeKerSelSubtitle(x_type, rho)))
              cowplot::save_plot(
                filename = files$kerplots[y_ind, rho_ind], plot = plot_kersel,
                ncol = length(pset_kersel_plots),
                base_height = height_kersel,
                base_asp = asp_kersel(n_cols = length(pset_kersel_plots)))
            }
          }
        }
      }
      if (x_type == "snp") {
        for (y_ind in seq_along(y_col_labels)) {
          # Check first if plot file exists
          if (!file.exists(files$kerplots[[y_ind]]) |
              overwrite_existing_plots) {
            plot_kersel <-
              makeKerSelPlots(
                plotdata_kersel[[y_ind]], x_type,
                theme_settings =
                  theme_kersel(plot_margin = margin(l = -20, b = 15)),
                title_settings =
                  title_kersel(joint_title = makeKerSelTitle(y_ind),
                               joint_subtitle = makeKerSelSubtitle(x_type)))
            cowplot::save_plot(
              filename = files$kerplots[[y_ind]], plot = plot_kersel,
              ncol = length(values_for_error_corr_strength),
              base_height = height_kersel,
              base_asp =
                asp_kersel(n_cols = length(values_for_error_corr_strength)))
          }
        }
      }
    }
  }
}