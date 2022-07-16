# Files for Plots
bench_plot_filename_stem <- paste0("b", num_permutations, "_m", num_replicates)
bench_file_kermat <-
  file.path(dir_plots_notitles_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_kermat.pdf"))
bench_file_stat <-
  file.path(dir_plots_notitles_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_stat.pdf"))
bench_file_phimr <-
  file.path(dir_plots_notitles_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_phimr.pdf"))
bench_file_amkat <-
  file.path(dir_plots_notitles_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_amkat.pdf"))
bench_file_cov <-
  file.path(dir_plots_notitles_runtime_benchmarking,
            paste0(bench_plot_filename_stem, "_cov.pdf"))

bench_plot_files_exist <-
  file.exists(bench_file_kermat) &
  file.exists(bench_file_stat) &
  file.exists(bench_file_phimr) &
  file.exists(bench_file_amkat)

# Create plots ------------------------------------------------------------

# Check first whether files already exist
if (!bench_plot_files_exist | overwrite_existing_plots) {

  load(bench_plotdata_file)

  ### CREATE PLOTS ###

  # Kernel Matrix
  if (!file.exists(bench_file_kermat) | overwrite_existing_plots) {
    plot_kermat <- makeKernelBenchPlots2(
      data_kermat,
      title_settings = title_bench_ker(
        reps_kermat,
        joint_title_size = 18,
        joint_subtitle_size = 14,
        joint_title_height = 0.12,
        title_overlay_margins = margin(t = 3, l = 7),
        joint_subtitle_height = 0.12,
        subtitle_overlay_margins = margin(t = -15, l = 7),
        joint_legend_height = 0.01,
        joint_legend_margins = margin(t = -10, b = -10)),
      theme_settings = theme_bench_ker(base_size = 16,
                                       fontsize_title = rel(10/11),
                                       # fontsize_x = rel(10/11),
                                       # fontsize_y = rel(10/11)
      )
    )
    cowplot::save_plot(
      filename = bench_file_kermat, plot = plot_kermat,
      ncol = 5,
      base_height = 4.1,
      # base_width = 2.1
      base_asp = 0.6
    )
  }

  # PhiMr Filter
  if (!file.exists(bench_file_phimr) | overwrite_existing_plots) {
    plot_phimr <- makePhimrBenchPlot(
      data_phimr, reps_phimr,
      theme_settings = theme_bench_phimr(aspect = 0.3)) +
      labs(title = NULL, subtitle = NULL) +
      guides(x = guide_axis(angle = 0))
    cowplot::save_plot(
      filename = bench_file_phimr, plot = plot_phimr,
      base_height = 2.75, base_width = 8)
  }

  # single-trait standardized statistic
  if (!file.exists(bench_file_stat) | overwrite_existing_plots) {
    plot_stat <-
      makeGenericBenchPlot(
        data_stat, reps_stat, 1e+6, "Median Time (ms)",
        NULL,
        NULL,
        theme_settings = theme_bench_phimr(aspect = 0.3)) +
      guides(x = guide_axis(angle = 0))
    cowplot::save_plot(
      filename = bench_file_stat, plot = plot_stat,
      base_width = 8, base_height = 2.5)
  }

  # covariate adjustment
  if (!file.exists(bench_file_cov) | overwrite_existing_plots) {
    plot_cov <-
      makeGenericBenchPlot(
        data_cov, reps_cov, 1e+3, "Median Time (\U00B5s)",
        NULL,
        NULL,
        theme_settings = theme_bench_phimr(aspect = 0.3)) +
      guides(x = guide_axis(angle = 0))
    cowplot::save_plot(
      filename = bench_file_cov, plot = plot_cov,
      base_width = 8, base_height = 2.3)
  }

  # AMKAT full test
  if (!file.exists(bench_file_amkat) | overwrite_existing_plots) {
    plot_amkat <- makeAmkatBenchPlots2(
      data = data_amkat, title_settings = title_bench_amkat(num_permutations))
    cowplot::save_plot(
      filename = bench_file_amkat, plot = plot_amkat,
      ncol = 2, base_height = 3.5, base_asp = 1)
  }
}