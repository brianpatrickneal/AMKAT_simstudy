
# Generic Plotting Functions ----------------------------------------------


# Function: construct multiple side-by-side violin plots using 'data',
#  where each side-by-side plot corresponds to a specific 2D slice of 'data';
# stack the plots vertically in a compound plot
# input 'data' is an array whose third dimension indexes observations
# input 'slice' specifies whether to use row slices or column slices
# input 'plot_titles' is a character vector whose length matches the dimension
#  of 'data' specified in 'slice' (one title per slice, i.e. per plot)
# input 'plot_subtitles' is analogous to 'plot_titles', but for plot subtitles
# other inputs are passed to makeViolinPlot()
makeViolinPlots2 <- function(data,
                             slice = 'rows', # one plot per row
                             variable_name,
                             plot_titles = NULL,
                             plot_subtitles = NULL,
                             x_axis_title = NULL,
                             y_axis_title = NULL,
                             color_scheme = "H",
                             theme_settings = theme_violin(),
                             title_settings = title_violin()
) {

  global_min <- min(data)
  global_max <- max(data)
  num_plots <- integer() #init
  plots <- list() #init

  if (slice %in% c("c", "col", "column", "columns")) {
    num_plots <- ncol(data)
    for (col_ind in 1:num_plots) {
      # Make side-by-side violin plot using current 2D column slice of array
      newplot <-
        makeViolinPlot(t(data[ , col_ind, ]),
                       variable_name,
                       plot_titles[[col_ind]],
                       plot_subtitles[[col_ind]],
                       x_axis_title,
                       y_axis_title,
                       y_axis_limits = c(global_min, global_max),
                       color_scheme = color_scheme
        ) + theme_settings +
        theme(axis.title.x = element_text(face = "bold"))
      if (col_ind < num_plots) { newplot <- newplot + xlab(NULL) }
      # Add plot to storage list
      plots <- c(plots, list(newplot))
    }
  }
  if (slice %in% c("r", "row", "rows")) {
    num_plots <- nrow(data)
    for (row_ind in 1:num_plots) {
      # Make side-by-side violin plot using current 2D column slice of array
      newplot <-
        makeViolinPlot(t(data[row_ind, , ]),
                       variable_name,
                       plot_titles[[row_ind]],
                       plot_subtitles[[row_ind]],
                       x_axis_title,
                       y_axis_title,
                       y_axis_limits = c(global_min, global_max),
                       color_scheme = color_scheme
        ) + theme_settings +
        theme(axis.title.x = element_text(face = "bold"))
      if (row_ind < num_plots) { newplot <- newplot + xlab(NULL) }
      # Add plot to storage list
      plots <- c(plots, list(newplot))
    }
  }

  # Create compound plot
  if (num_plots == 2) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     ncol = 1,
                                     labels = rep("", 2),
                                     align = "vh")
  } else if (num_plots == 3) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     plots[[3]],
                                     ncol = 1,
                                     labels = rep("", 3),
                                     align = "vh")
  } else if (num_plots == 4) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     plots[[3]], plots[[4]],
                                     ncol = 1,
                                     labels = rep("", 4),
                                     align = "vh")
  } else if (num_plots == 5) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     plots[[3]], plots[[4]], plots[[5]],
                                     ncol = 1,
                                     labels = rep("", 5),
                                     align = "vh")
  } else if (num_plots == 6) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     plots[[3]], plots[[4]],
                                     plots[[5]], plots[[6]],
                                     ncol = 1,
                                     labels = rep("", 6),
                                     align = "vh")
  } else if (num_plots == 7) {
    joint_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                     plots[[3]], plots[[4]],
                                     plots[[5]], plots[[6]], plots[[7]],
                                     ncol = 1,
                                     labels = rep("", 7),
                                     align = "vh")
  } else if (num_plots > 7) {
    stop("Too many plots (slices, i.e., group values), maximum of 7 supported")
  }

  # Overlay title, subtitle and legend onto joint plot and return
  return(joint_plot)
}




# Titles & Subtitles ------------------------------------------------------

overlayJointPlotElements2 <- function(joint_plot, title_settings,
                                      theme_settings, legend_from) {

  # Extract legend from an individual plot
  joint_legend <- cowplot::get_legend(
    legend_from +
      theme(legend.box.margin = title_settings$joint_legend_margins))
  if (theme_settings$legend.position == "top") {
    out <- cowplot::plot_grid(
      joint_legend, joint_plot, ncol = 1,
      rel_heights = c(title_settings$joint_legend_height, 1))
  } else if (theme_settings$legend.position == "right") {
    out <- cowplot::plot_grid(
      joint_plot, joint_legend, ncol = 2,
      rel_widths = c(1, title_settings$joint_legend_width))
  } else if (theme_settings$legend.position == "left") {
    out <- cowplot::plot_grid(
      joint_legend, joint_plot, ncol = 2,
      rel_widths = c(title_settings$joint_legend_width, 1))
  } else { # default to legend position bottom
    out <- cowplot::plot_grid(
      joint_plot, joint_legend, ncol = 1,
      rel_heights = c(1, title_settings$joint_legend_height))
  }
  return(out)
}




# Power -------------------------------------------------------------------


makeCompoundPowerPlot2 <- function(
  data = plot_data_main, test_methods = "comparison",
  color_scheme = colors_power(n_colors = length(levels(data$method))),
  shape_scheme = shapes_power,
  line_size = line_size_power,
  point_size = point_size_power,
  y_scale_max = 1,
  theme_settings = theme_power(),
  title_settings = title_power(
    joint_title_height = powerTitleHeight(length(unique(data$corr))),
    joint_subtitle_height = powerSubtitleHeight(length(unique(data$corr))))) {

  if (test_methods == "comparison") {
    legend_title <- "Testing Method"
  } else if (test_methods == "amkat") {
    legend_title <- "Number of test stats\nused to estimate\nP-value with PhiMr"
  }
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$corr))
  sample_sizes <- sort(unique(data$sample_size))
  corr_labels <- rep('NA', length(corr_values))
  for (i in seq_along(corr_values)) {
    if (corr_values[[i]] == 0) {
      corr_labels[[i]] <- 'Uncorrelated error components'
    } else {
      corr_labels[[i]] <- expression(paste(
        'Correlated error components (', rho, ' = 0.5)'))
    }
  }
  plot_sparse_corr1 <- makePowerPlot(
    data, corr_values, corr_labels, corr_index = 1, signal_values[[1]],
    legend_title, color_scheme, shape_scheme, line_size, point_size,
    y_scale_max, theme_settings, sample_sizes)
  plot_dense_corr1 <- makePowerPlot(
    data, corr_values, corr_labels, corr_index = 1, signal_values[[2]],
    legend_title, color_scheme, shape_scheme, line_size, point_size,
    y_scale_max, theme_settings, sample_sizes)

  if (length(corr_values) == 2) {
    # Four-plot layout
    plot_sparse_corr2 <- makePowerPlot(
      data, corr_values, corr_labels, corr_index = 2, signal_values[[1]],
      legend_title, color_scheme, shape_scheme, line_size, point_size,
      y_scale_max, theme_settings, sample_sizes)
    plot_dense_corr2 <- makePowerPlot(
      data, corr_values, corr_labels, corr_index = 2, signal_values[[2]],
      legend_title, color_scheme, shape_scheme, line_size, point_size,
      y_scale_max, theme_settings, sample_sizes)
    joint_plot <- cowplot::plot_grid(
      plot_sparse_corr1 + theme(legend.position = "none"),
      plot_sparse_corr2 + theme(legend.position = "none") +
        ylab(NULL) + labs(title = NULL),
      plot_dense_corr1 + theme(legend.position = "none") ,
      plot_dense_corr2 + theme(legend.position = "none") +
        ylab(NULL) + labs(title = NULL),
      ncol = 2, labels = rep("", 4), align = "vh")
  } else if (length(corr_values) == 1) {
    # Two-plot layout
    joint_plot <- cowplot::plot_grid(
      plot_sparse_corr1 + theme(legend.position = "none"),
      plot_dense_corr1 + theme(legend.position = "none"),
      ncol = 2, labels = rep("", 2), align = "v")
  }
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(joint_plot, title_settings, theme_settings,
                                   legend_from = plot_sparse_corr1))

}

makeCompoundPowerBarPlot2 <- function(
  data = plot_data_amkat, test_methods = "amkat",
  color_scheme = colors_powerbar_amkat(n_colors = length(levels(data$method))),
  y_scale_max = 1,
  theme_settings = theme_power(),
  title_settings = title_power(
    joint_title = "Simulated Power: Comparison of AMKAT Variations",
    joint_title_height = powerTitleHeight(length(unique(data$corr))),
    joint_subtitle_height = powerSubtitleHeight(length(unique(data$corr))),
    joint_legend_width = 0.2)) {

  if (test_methods == "comparison") {
    legend_title <- "Testing Method"
  } else if (test_methods == "amkat") {
    legend_title <- "Number of test stats\nused to estimate\nP-value with PhiMr"
  }
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$corr))
  group_labels <- levels(data$method)
  sample_sizes <- sort(unique(data$sample_size))
  corr_labels <- rep('NA', length(corr_values))
  for (i in seq_along(corr_values)) {
    if (corr_values[[i]] == 0) {
      corr_labels[[i]] <- 'Uncorrelated error components'
    } else {
      corr_labels[[i]] <- expression(paste(
        'Correlated error components (', rho, ' = 0.5)'))
    }
  }
  plot_sparse_corr1 <- makePowerBarPlot(
    data, corr_values, corr_labels, corr_index = 1, signal_values[[1]],
    legend_title, color_scheme, group_labels, y_scale_max, theme_settings,
    sample_sizes)
  plot_dense_corr1 <- makePowerBarPlot(
    data, corr_values, corr_labels, corr_index = 1, signal_values[[2]],
    legend_title, color_scheme, group_labels, y_scale_max, theme_settings,
    sample_sizes)

  if (length(corr_values) == 2) { # Four-plot layout
    plot_sparse_corr2 <- makePowerBarPlot(
      data, corr_values, corr_labels, corr_index = 2, signal_values[[1]],
      legend_title, color_scheme, group_labels, y_scale_max, theme_settings,
      sample_sizes)
    plot_dense_corr2 <- makePowerBarPlot(
      data, corr_values, corr_labels, corr_index = 2, signal_values[[2]],
      legend_title, color_scheme, group_labels, y_scale_max, theme_settings,
      sample_sizes)
    joint_plot <- cowplot::plot_grid(
      plot_sparse_corr1 + theme(legend.position = "none"),
      plot_sparse_corr2 + theme(legend.position = "none") +
        ylab(NULL) + labs(title = NULL),
      plot_dense_corr1 + theme(legend.position = "none") ,
      plot_dense_corr2 + theme(legend.position = "none") +
        ylab(NULL) + labs(title = NULL),
      ncol = 2, labels = rep("", 4), align = "vh")
  }

  if (length(corr_values) == 1) { # Two-plot layout
    # Two-plot layout
    joint_plot <- cowplot::plot_grid(
      plot_sparse_corr1 + theme(legend.position = "none"),
      plot_dense_corr1 + theme(legend.position = "none"),
      ncol = 2, labels = rep("", 2), align = "v")
  }
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(joint_plot, title_settings, theme_settings,
                                   legend_from = plot_sparse_corr1))
}



# Adaptive Across ---------------------------------------------------------

# Generate list of file paths for plot data and plots
adaptiveAcrossPlotFiles2 <- function() {

  # file path for output plot data
  part1 <- paste0('m', num_replicates, '_', x_type, '_',
                  error_distribution, '_s', signal_strength * 100)
  if (x_type == 'snp') {
    stem <- paste0(part1, '_sc-', signal_correlation)
  } else { stem <- part1 }
  plotdata_file <- file.path(dir_data_adaptive_across,
                             paste0(stem, '_plotdata.Rdata'))

  if (x_type == "cts") {
    kerplot_filename_vec <-
      paste0(stem, '_c', values_for_error_corr_strength * 100)
    # matrix of file paths for kernel selection plots
    #  cols index correlation, rows index Y variable
    kerplot_files <-
      sapply(
        kerplot_filename_vec,
        FUN = function(kerplot_filestem) {
          file.path(dir_plots_notitles_kernel_selection,
                    paste0(kerplot_filestem, '_', y_col_labels, '.pdf')) })
  }
  if (x_type == "snp") {
    # vector of file paths for kernel selection plots
    kerplot_files <- file.path(dir_plots_notitles_kernel_selection,
                               paste0(stem, '_', y_col_labels, '.pdf'))
  }
  phimrplot_retention_file <-
    file.path(dir_plots_notitles_feature_selection, paste0(stem, '_keeprates.pdf'))

  phimrplot_density_file <-
    file.path(dir_plots_notitles_feature_selection, paste0(stem, '_density.pdf'))

  return(list("plotdata" = plotdata_file,
              "kerplots" = kerplot_files, # character matrix/vector
              "phimrplot_retention" = phimrplot_retention_file,
              "phimrplot_density" = phimrplot_density_file,
              "kerplots_exist" =
                sum(file.exists(kerplot_files)) == length(kerplot_files)))
}


makeKerSelPlots2 <- function(
  data, x_type,
  rho = NULL, # only used for x_type cts
  p_set = pset_kersel_plots, # only used for x_type cts
  color_scheme = colors_kersel(n_colors = 5),
  shape_scheme = all_shapes,
  line_size = line_size_kersel,
  point_size = point_size_kersel,
  theme_settings = theme_kersel(),
  title_settings) {

  signal_values <- sort(unique(data$signals))
  sample_sizes <- sort(unique(data$samplesize)) # for x axis label increments
  if (x_type == "cts") {
    plot11 <- makeKerSelPlot(
      data, rho, signal_values[[1]], p_set[[1]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes)
    plot12 <- makeKerSelPlot(
      data, rho, signal_values[[1]], p_set[[2]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL) +
      ylab(NULL)
    plot13 <- makeKerSelPlot(
      data, rho, signal_values[[1]], p_set[[3]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL) +
      ylab(NULL)
    plot21 <- makeKerSelPlot(
      data, rho, signal_values[[2]], p_set[[1]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes)
    plot22 <- makeKerSelPlot(
      data, rho, signal_values[[2]], p_set[[2]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL) +
      ylab(NULL)
    plot23 <- makeKerSelPlot(
      data, rho, signal_values[[2]], p_set[[3]], color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL) +
      ylab(NULL)
    joint_plot <- cowplot::plot_grid(
      plot11 + theme(legend.position = "none"),
      plot12 + theme(legend.position = "none"),
      plot13 + theme(legend.position = "none"),
      plot21 + theme(legend.position = "none"),
      plot22 + theme(legend.position = "none"),
      plot23 + theme(legend.position = "none"),
      ncol = 3, labels = rep("", 6), align = "vh")
  }
  if (x_type == "snp") {
    plot11 <- makeKerSelPlot(
      data, rho = 0, signal_values[[1]], 567, color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes,
      subtitle = 'Uncorrelated error components')
    plot12 <- makeKerSelPlot(
      data, rho = 0.5, signal_values[[1]], 567, color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL,
      subtitle =
        expression(paste(
          # 'Correlated error components (', rho, ' = 0.5)'))) +
          'Correlated error components'))) +
      ylab(NULL)
    plot21 <- makeKerSelPlot(
      data, rho = 0, signal_values[[2]], 567, color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes,
      subtitle = 'Uncorrelated error components')
    plot22 <- makeKerSelPlot(
      data, rho = 0.5, signal_values[[2]], 567, color_scheme, shape_scheme,
      line_size, point_size, theme_settings, sample_sizes, title = NULL,
      subtitle =
        expression(paste(
          # 'Correlated error components (', rho, ' = 0.5)'))) +
          'Correlated error components'))) +
      ylab(NULL)
    joint_plot <- cowplot::plot_grid(
      plot11 + theme(legend.position = "none"),
      plot12 + theme(legend.position = "none"),
      plot21 + theme(legend.position = "none"),
      plot22 + theme(legend.position = "none"),
      ncol = 2, labels = rep("", 4), align = "vh")
  }
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(joint_plot, title_settings, theme_settings,
                                   legend_from = plot11))

}

makePhimrRetentionPlots2 <- function(data,
                                     theme_settings = theme_phimr()) {

  data <- data[plotdata_phimr$datatype %in% c('signal', 'noise'), ]
  data$linegroup <- paste0(data$datatype, data$featdim)
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$rho))
  sample_sizes <- sort(unique(data$samplesize))
  plot_sparse_corr1 <- phimrRetentionPlot2(
    data, corr_values[[1]], signal_values[[1]], sample_sizes)
  plot_dense_corr1 <- phimrRetentionPlot2(
    data, corr_values[[1]], signal_values[[2]], sample_sizes)
  plot_sparse_corr2 <- phimrRetentionPlot2(
    data, corr_values[[2]], signal_values[[1]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  plot_dense_corr2 <- phimrRetentionPlot2(
    data, corr_values[[2]], signal_values[[2]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  joint_plot <- cowplot::plot_grid(
    plot_sparse_corr1 + theme(legend.position = "none"),
    plot_sparse_corr2 + theme(legend.position = "none"),
    plot_dense_corr1 + theme(legend.position = "none"),
    plot_dense_corr2 + theme(legend.position = "none"),
    ncol = 2, labels = rep("", 4), align = "vh")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(
    joint_plot, title_settings = title_phimr(),
    theme_settings, legend_from = plot_sparse_corr1))
}

phimrRetentionPlot2 <- function(data, corr_value, num_signals, x_breaks) {
  makeLinePlot(
    data[which((data$rho == corr_value) & (data$signals == num_signals)), ],
    xvar = "samplesize", yvar = "value", groupvar = "linegroup",
    color_scheme = colors_phimr(n_colors = length(unique(data$featdim))),
    shape_scheme = shapes_phimr,
    plot_title = paste0(num_signals, '-variable signal set'),
    plot_subtitle = switch(
      as.character(corr_value),
      "0" = "Uncorrelated error components",
      expression(paste(
        'Correlated error components (', rho, ' = 0.5)'))),
    plot_caption = NULL,
    x_axis_title = "Sample Size",
    y_axis_title = "Share of Variables Kept",
    legend_title = NULL,
    line_size = line_size_phimr, point_size = point_size_phimr,
    theme_settings = theme_phimr(
      plot_margin = margin(l = 5, b = 10)
    ),
    color_by = "featdim", shapes_by = "datatype",
    color_legend_title = "feature dim.", shape_legend_title = "variable type"
  ) +
    scale_y_continuous(breaks = seq(from = 0, to = 1,
                                    by = 0.1),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = x_breaks)
}


makePhimrDensityPlots2 <- function(data,
                                   theme_settings = theme_phimr()) {

  data <- data[plotdata_phimr$datatype == 'density', ]
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$rho))
  sample_sizes <- sort(unique(data$samplesize))
  plot_sparse_corr1 <- phimrDensityPlot2(
    data, corr_values[[1]], signal_values[[1]], sample_sizes)
  plot_dense_corr1 <- phimrDensityPlot2(
    data, corr_values[[1]], signal_values[[2]], sample_sizes)
  plot_sparse_corr2 <- phimrDensityPlot2(
    data, corr_values[[2]], signal_values[[1]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  plot_dense_corr2 <- phimrDensityPlot2(
    data, corr_values[[2]], signal_values[[2]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  joint_plot <- cowplot::plot_grid(
    plot_sparse_corr1 + theme(legend.position = "none"),
    plot_sparse_corr2 + theme(legend.position = "none"),
    plot_dense_corr1 + theme(legend.position = "none") ,
    plot_dense_corr2 + theme(legend.position = "none"),
    ncol = 2, labels = rep("", 4), align = "vh")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(
    joint_plot,
    title_settings = title_phimr(
      joint_title = "PhiMr Filter Effect on Signal Density",
      joint_subtitle = makePhimrSubtitle("density"),
      joint_subtitle_height = 0.12),
    theme_settings, legend_from = plot_sparse_corr1))
}

phimrDensityPlot2 <- function(data, corr_value, num_signals, x_breaks) {
  makeLinePlot(
    data[which((data$rho == corr_value) & (data$signals == num_signals)), ],
    xvar = "samplesize", yvar = "value", groupvar = "featdim",
    color_scheme = colors_phimr(n_colors = length(unique(data$featdim))),
    shape_scheme = all_shapes,
    plot_title = paste0(num_signals, '-variable signal set'),
    plot_subtitle = switch(
      as.character(corr_value),
      "0" = "Uncorrelated error components",
      "0.5" =
        expression(paste(
          'Correlated error components (', rho, ' = 0.5)'))),
    plot_caption = NULL,
    x_axis_title = "Sample Size",
    y_axis_title = "Signal Density, After / Before",
    legend_title = "feature dim.",
    line_size = line_size_phimr, point_size = point_size_phimr,
    theme_settings = theme_phimr(
      plot_margin = margin(l = 5, b = 10)
    )
  ) +
    scale_y_continuous(breaks = seq(from = 1, to = 2,
                                    by = 0.1),
                       limits = c(1, 2)) +
    scale_x_continuous(breaks = x_breaks)
}

# Adaptive Within ---------------------------------------------------------

# Generate list of file paths for plot data and plots
pvaluePlotFiles2 <- function() {

  source(file.path(dir_src, 'define_scenario_filename.R'))

  file_data_stats <-
    file.path(dir_data_adaptive_within,
              paste0(scenario_filename_stem, '_raw.Rdata'))
  file_data_pvalues <-
    file.path(dir_data_adaptive_within,
              paste0(scenario_filename_stem, '_pvalues.Rdata'))
  file_plot_statdistr <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_stat_distr.pdf"))
  file_plot_violinQ <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_violin_Q.pdf"))
  file_plot_violinB <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_violin_B.pdf"))
  file_plot_sdline <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_SDlineplot.pdf"))
  file_plot_meanline <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_meanlineplot.pdf"))
  file_plot_phimr <-
    file.path(dir_plots_notitles_adaptive_within,
              paste0(scenario_filename_stem, "_phimr.pdf"))

  return(list("stats" = file_data_stats,
              "pvalues" = file_data_pvalues,
              "plot_stats" = file_plot_statdistr,
              "plot_violinQ" = file_plot_violinQ,
              "plot_violinB" = file_plot_violinB,
              "plot_sd" = file_plot_sdline,
              "plot_mean" = file_plot_meanline,
              "plot_phimr" = file_plot_phimr,
              "plots_exist" =
                sum(file.exists(
                  file_plot_statdistr, file_plot_violinQ, file_plot_violinB,
                  file_plot_sdline, file_plot_meanline, file_plot_phimr)) == 6))
}

# Input stats from raw simulated data
# Create left and right plots for test/perm statistic distributions
# x_offset_label_meanstat: x position of "mean test stat" label on right plot
makeStatDistrPlots2 <- function(
  test_stats = test_stats, perm_stats = perm_stats,
  perm_stats_no_filter = perm_stats_no_filter,
  test_stat_no_filter = test_stat_no_filter,
  theme_settings = theme_statDistr(),
  title_settings = title_statDistr(),
  annotation_size = 3) {
  minval <- min(test_stats, perm_stats,
                test_stat_no_filter, perm_stats_no_filter)
  maxval <- max(test_stats, perm_stats,
                test_stat_no_filter, perm_stats_no_filter)
  mean_test_stat <- mean(test_stats)

  # Extend shorter vectors to length of perm_stats using NA values
  test_stats_long <- rep(NA, times = length(perm_stats))
  test_stats_long[1:length(test_stats)] <- as.vector(test_stats)
  test_stats_no_filter_long <- rep(test_stat_no_filter, length(perm_stats))

  # Create data frame for each plot
  data_stats_no_filter <- data.frame(cbind(test_stats_no_filter_long,
                                           as.vector(perm_stats_no_filter)))
  colnames(data_stats_no_filter) <-
    c('Test statistic', paste0('Permutation statistics\n(',
                               num_replicates * num_permutations, ' values)'))

  data_stats <- data.frame(cbind(test_stats_long, as.vector(perm_stats)))
  colnames(data_stats) <-
    c(paste0('Test statistics\n(', num_replicates * max(Q_values), ' values)'),
      paste0('Permutation statistics\n(',
             num_replicates * num_permutations, ' values)'))

  # Convert data to long format
  plot_data_stats_no_filter <- tidyr::pivot_longer(
    data_stats_no_filter, cols = everything(),
    names_to = 'group', values_to = 'value')
  plot_data_stats_no_filter$group <- factor(
    plot_data_stats_no_filter$group, levels = colnames(data_stats_no_filter))
  plot_data_stats <- tidyr::pivot_longer(
    data_stats, cols = everything(),
    names_to = 'group', values_to = 'value')
  plot_data_stats$group <- factor(
    plot_data_stats$group, levels = colnames(data_stats))

  # Create left and right plots
  plot_left <- makeStatDistrPlot(
    plot_data_stats_no_filter, 'Without Filter', minval, maxval,
    test_stat_no_filter, 'observed\nvalue', line_label_position = 1,
    plot_subtitle = 'Test statistic is deterministic on fixed data',
    annotation_size, theme_settings)
  plot_right <- makeStatDistrPlot(
    plot_data_stats, 'With PhiMr Filter', minval, maxval,
    mean_test_stat, 'mean\ntest\nstat\n', line_label_position = 2.45,
    plot_subtitle = 'Test statistic varies on fixed data',
    annotation_size, theme_settings) + ylab(NULL)

  # Create compound plot
  joint_plot <- cowplot::plot_grid(plot_left, plot_right,
                                   ncol = 2, labels = c("", ""), align = "vh")
  # Overlay title, subtitle and legend onto joint plot and return
  return(joint_plot)
}

# pvalues is an array of pvalues
makeViolinQPlots2 <- function(
  pvalues, group_B_values = B_values_violinQ,
  theme_settings = theme_violinQ(),
  title_settings = title_violinQ(num_replicates),
  color_code = color_code_violinQ) {

  # create temp copy of input data and rename columns
  all_B_values <- as.numeric(dimnames(pvalues)[[1]])
  B_indices <- which(all_B_values %in% group_B_values)
  temp_data <- pvalues[B_indices, , ]
  dimnames(temp_data)[[2]][
    dimnames(temp_data)[[2]] == "NF"] <- "PF-0 (no filter)"
  dimnames(temp_data)[[2]] <-
    sapply(dimnames(temp_data)[[2]], # strip prefix "PF-"
           function(x) { substr(x, start = 4, stop = nchar(x)) })

  out <- makeViolinPlots2(
    data = temp_data,
    slice = 'rows', # one plot per row
    variable_name = "Pval",
    plot_titles = NULL,
    plot_subtitles =
      paste0(B_values_violinQ, " permutation statistics per P-value"),
    x_axis_title =
      "Test Statistics Used for P-value Estimate With PhiMr Filter",
    y_axis_title = "P-value",
    color_scheme = color_code,
    theme_settings = theme_settings,
    title_settings = title_settings
  )

  return(out)
}

# pvalues is an array of pvalues
makeViolinBPlots2 <- function(
  pvalues, group_Q_values = Q_values_violinB,
  theme_settings = theme_violinB(),
  title_settings = title_violinB(num_replicates),
  color_code = color_code_violinB) {

  # create temp copy of input data and rename columns
  all_Q_values <- dimnames(pvalues)[[2]]
  Q_indices <- c(1, which(all_Q_values %in% paste0("PF-", group_Q_values)))

  out <- makeViolinPlots2(
    data = pvalues[ , Q_indices, ],
    slice = 'columns', # one plot per row
    variable_name = "Pval",
    plot_titles = NULL,
    plot_subtitles = c("No filter",
                       "PhiMr filter with 1 test statistic per P-value",
                       paste0("PhiMr filter with ", group_Q_values[-1],
                              " test statistics per P-value")),
    x_axis_title = "Permutation statistics per P-value",
    y_axis_title = "P-value",
    color_scheme = color_code,
    theme_settings = theme_settings,
    title_settings = title_settings
  )

  return(out)
}



# Input 'pvalues' is a 3D array:
#   - rows index number of perms per P-value (B)
#   - cols index number of test stats per P-value (Q)
#   - third dimension indexes observations/replicates
# Make side-by-side line plots of P-value sample stat vs Q (left) and B (right)
# Return compound plot created using cowplot::plot_grid
makePVLinePlots2 <- function(pvalues,
                             num_replicates,
                             summary_stat = sd,
                             # groups for plot vs B
                             values_Q = Q_values_line, # line groups
                             colors_Q = Q_colors_line, # line colors
                             shapes_Q = Q_shapes_line, # point shapes
                             # groups for plot vs Q
                             values_B = B_values_line,
                             colors_B = B_colors_line,
                             shapes_B = B_shapes_line,
                             line_size = line_size_pv,
                             point_size = point_size_pv,
                             theme_settings = theme_pvline(),
                             title_settings = title_pvline()
) {

  # Y-axis title based on type of summary stat
  if (summary_stat(c(1, 1)) == 0) { #sd
    y_axis_title = "P-value Standard Deviation"
  } else if (summary_stat(c(1, 1)) == 1) { #mean
    y_axis_title = "Mean P-value"
  } else { y_axis_title = paste(expression(summary_stat))}

  # Indices for groups
  B_indices <- # groups for plot vs Q
    which(as.numeric(dimnames(pvalues)[[1]]) %in% values_B)
  Q_indices <- # groups for plot vs B
    c(1, which(dimnames(pvalues)[[2]] %in% paste0("PF-", values_Q)))

  # Format data for plotting
  data_Q <- # data for summary stat vs Q
    preparePVLinePlotData(pvalues[B_indices, , ], # groups = B
                          slice = "rows",
                          summary_stat)

  data_B <- # data for summary stat vs B
    preparePVLinePlotData(pvalues[-c(1, 2), Q_indices, ], # groups = Q
                          slice = "columns",
                          summary_stat)

  lineplot_Q <- # plot of summary stat vs Q
    makeLinePlot(
      data = data_Q, xvar = "x_variable", yvar = "stat", groupvar = "group",
      color_scheme = colors_B, shape_scheme = shapes_B,
      plot_title = "As a function of the # of test statistics",
      plot_subtitle = paste0("Each point represents a sample of ",
                             num_replicates, " P-values"),
      plot_caption = "(0 corresponds to testing without PhiMr filter)",
      x_axis_title = "Number of test statistics used with PhiMr",
      y_axis_title = y_axis_title,
      legend_title = "# perms", line_size, point_size, theme_settings) +
    scale_x_continuous(trans = "log2", breaks = unique(data_Q$x_variable),
                       minor_breaks = NULL)

  lineplot_B <-  # plot of summary stat vs B
    makeLinePlot(
      data = data_B, xvar = "x_variable", yvar = "stat", groupvar = "group",
      color_scheme = colors_Q, shape_scheme = shapes_Q,
      plot_title = "As a function of the # of permutation statistics",
      plot_subtitle = paste0("Each point represents a sample of ",
                             num_replicates, " P-values"),
      plot_caption = NULL,
      x_axis_title = "Number of permutations",
      y_axis_title = y_axis_title,
      legend_title = "# test stats", line_size, point_size, theme_settings) +
    scale_x_continuous(breaks = unique(data_B$x_variable))

  # Create compound plot
  joint_plot <- cowplot::plot_grid(lineplot_Q, lineplot_B,
                                   ncol = 2, labels = c("", ""), align = "hv")

  # Overlay title, subtitle and legend onto joint plot and return
  return(joint_plot)
}

makePhimrHistograms2 <- function(keeprates, signal_indices) {

  maxcount_signal <-
    max(
      table(cut(keeprates[signal_indices], breaks = seq(-0.05, 1, by = 0.05))))
  maxcount_noise <-
    max(
      table(cut(keeprates[-signal_indices], breaks = seq(-0.05, 1, by = 0.05))))
  maxcount_global <- max(maxcount_noise, maxcount_signal)
  plot_signal <- makePhimrHistogram(keeprates[signal_indices], "signal",
                                    maxcount_global)
  plot_noise <- makePhimrHistogram(keeprates[-signal_indices], "noise",
                                   maxcount_global) + ylab(NULL)
  return(cowplot::plot_grid(plot_signal, plot_noise,
                            ncol = 2, labels = c("", ""), align = "hv"))
}



# Runtime Benchmarking ----------------------------------------------------


# Data is an array; 3rd dim indexes the plots; rows index n, cols index p
makeKernelBenchPlots2 <- function(data = data_kermat,
                                  timefactor = timefactor_ker,
                                  y_axis_title = timelabel_ker,
                                  line_size = line_size_bench_ker,
                                  point_size = point_size_bench_ker,
                                  theme_settings = theme_bench_ker(),
                                  title_settings = title_bench_ker(),
                                  color_scheme =
                                    colors_bench(n_colors = dim(data)[[1]]),
                                  shape_scheme = all_shapes) {

  # limits and ticks for Y axis scale
  global_min <- 2^floor(log2(min(data) / timefactor))
  global_max <- 2^ceiling(log2(max(data) / timefactor))
  y_ticks <- 2^(0:log2(global_max))
  ylims <- c(global_min, global_max)

  kerplot_l <- makeBenchPlot(
    data[, , 1], title = "Linear Kernel",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme)
  kerplot_q <- makeBenchPlot(
    data[, , 2], title = "Quadratic Kernel",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme) + ylab(NULL)
  kerplot_g <- makeBenchPlot(
    data[, , 3], title = "Gaussian Kernel",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme) + ylab(NULL)
  kerplot_e <- makeBenchPlot(
    data[, , 4], title = "Exponential Kernel",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme) + ylab(NULL)
  kerplot_i <- makeBenchPlot(
    data[, , 5], title = "IBS Kernel",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme) + ylab(NULL)

  # Create compound plot
  joint_plot <- cowplot::plot_grid(kerplot_l + theme(legend.position = "none"),
                                   kerplot_q + theme(legend.position = "none"),
                                   kerplot_g + theme(legend.position = "none"),
                                   kerplot_e + theme(legend.position = "none"),
                                   kerplot_i + theme(legend.position = "none"),
                                   ncol = 5, labels = rep("", 5), align = "hv")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(joint_plot, title_settings, theme_settings,
                                   legend_from = kerplot_l))
}



# Data is an array; 3rd dim indexes the plots; rows index n, cols index p
makeAmkatBenchPlots2 <- function(data = data_amkat,
                                 timefactor = timefactor_amkat,
                                 y_axis_title = timelabel_amkat,
                                 line_size = line_size_bench_amkat,
                                 point_size = point_size_bench_amkat,
                                 theme_settings = theme_bench_amkat(),
                                 title_settings = title_bench_amkat(),
                                 color_scheme =
                                   colors_bench(n_colors = dim(data)[[1]]),
                                 shape_scheme = all_shapes) {

  # limits and ticks for Y axis scale
  global_min <- 2^floor(log2(min(data) / timefactor))
  global_max <- 2^ceiling(log2(max(data) / timefactor))
  y_ticks <- round(2^(log2(global_min):log2(global_max)), digits = 2)
  ylims <- c(global_min, global_max)

  plot_l <- makeBenchPlot(
    data = data[, , 1], title = "AMKAT Without Filter",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme)
  plot_r <- makeBenchPlot(
    data = data[, , 2], title = "AMKAT With PhiMr Filter",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme,
    subtitle = "P-value estimated using 50 test statistics") + ylab(NULL)

  # Create compound plot
  joint_plot <- cowplot::plot_grid(plot_l + theme(legend.position = "none"),
                                   plot_r + theme(legend.position = "none"),
                                   ncol = 2, labels = c("", ""), align = "hv")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements2(joint_plot, title_settings, theme_settings,
                                   legend_from = plot_l))
}
