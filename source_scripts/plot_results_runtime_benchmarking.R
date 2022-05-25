
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

  # single-trait standardized statistic
  if (!file.exists(bench_file_stat) | overwrite_existing_plots) {
    plot_stat <-
      makeGenericBenchPlot(
        data_stat, reps_stat, 1e+6, "Median Time (ms)",
        "Time to Compute\nStandardized Single-Trait Statistic",
        paste0(
          "4 response variables; continuous features\nMedian time over ",
          reps_stat, " executions of the .estimateSignalToNoise()\nfunction from the AMKAT R package",
          "\nComputation does not include kernel matrix"),
        theme_settings = theme_bench_phimr(aspect = 0.5))
    cowplot::save_plot(
      filename = bench_file_stat, plot = plot_stat,
      base_height = 7, base_asp = 0.8)
  }

  # covariate adjustment
  if (!file.exists(bench_file_cov) | overwrite_existing_plots) {
    plot_cov <-
      makeGenericBenchPlot(
        data_cov, reps_cov, 1e+3, "Median Time (\U00B5s)",
        "Time to Perform Covariate Adjustment",
        paste0(
          "4 response variables; 2 covariates; continuous features\nMedian time over ",
          reps_cov, " executions of the .fitAmkatNullModel()\nfunction from the AMKAT R package"),
        theme_settings = theme_bench_phimr(aspect = 0.5))
    cowplot::save_plot(
      filename = bench_file_cov, plot = plot_cov,
      base_height = 7, base_asp = 0.8)
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