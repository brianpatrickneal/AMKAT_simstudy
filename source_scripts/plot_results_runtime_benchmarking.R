
# Create plots ------------------------------------------------------------

# Check first whether files already exist
if (!bench_plot_files_exist | overwrite_existing_plots) {

  load(bench_plotdata_file)

  ### CREATE PLOTS ###

  # Kernel Matrix
  if (!file.exists(bench_file_kermat) | overwrite_existing_plots) {
    plot_kermat <- makeKernelBenchPlots(
      data_kermat, title_settings = title_bench_ker(reps_kermat)
    )
    cowplot::save_plot(
      filename = bench_file_kermat, plot = plot_kermat,
      ncol = 5,
      base_height = height_bench_ker,
      base_asp = asp_bench_ker)
  }

  # PhiMr Filter
  if (!file.exists(bench_file_phimr) | overwrite_existing_plots) {
    plot_phimr <- makePhimrBenchPlot(data_phimr, reps_phimr)
    cowplot::save_plot(
      filename = bench_file_phimr, plot = plot_phimr,
      base_height = height_bench_phimr, base_asp = asp_bench_phimr)
  }

  # AMKAT full test
  if (!file.exists(bench_file_amkat) | overwrite_existing_plots) {
    plot_amkat <- makeAmkatBenchPlots(
      data = data_amkat, title_settings = title_bench_amkat(num_permutations))
    cowplot::save_plot(
      filename = bench_file_amkat, plot = plot_amkat,
      ncol = 2, base_height = height_bench_amkat, base_asp = asp_bench_amkat)
  }
}