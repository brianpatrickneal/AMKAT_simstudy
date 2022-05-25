
# Generic Plotting Functions ----------------------------------------------


# data is a long-form data frame with x, y and groupvar columns
# groupvar is assumed to be a factor
# a separate line will be plotted for each unique value of groupvar
makeLinePlot <- function(
  data, xvar, yvar, groupvar,
  color_scheme = all_colors,
  shape_scheme = all_shapes,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_caption = NULL,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,
  line_size = 0.3,
  point_size = 1.75,
  theme_settings = theme_lineplot(),
  color_by = groupvar,
  shapes_by = groupvar,
  color_legend_title = legend_title,
  shape_legend_title = legend_title
) {
  out <-
    ggplot2::ggplot(
      data,
      aes(x = data[, xvar],
          y = data[, yvar],
          group = data[, groupvar],
          color = data[, color_by],
          shape = data[, shapes_by])
    ) +
    geom_line(aes(color = data[, color_by]),
              size = line_size) +
    geom_point(aes(shape = data[, shapes_by],
                   color = data[, color_by]),
               size = point_size) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         x = x_axis_title,
         y = y_axis_title,
         caption = plot_caption) +
    guides(color = guide_legend(title = color_legend_title),
           shape = guide_legend(title = shape_legend_title)) +
    scale_shape_manual(values = shape_scheme) +
    scale_color_manual(values = color_scheme) +
    theme_settings

  return(out)
}

# create grouped barplots where 'xvar' is the categorical variable and
# 'groupvar' determines the bar groups
# 'data' is a long-form data frame with x, y and groupvar columns
# groupvar is assumed to be a factor
makeBarPlot <- function(
  data, xvar, yvar, groupvar, group_labels,
  color_scheme = all_colors,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_caption = NULL,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,
  theme_settings = theme_lineplot()
) {
  out <-
    ggplot2::ggplot(
      data,
      aes(x = data[, xvar],
          y = data[, yvar],
          group = data[, groupvar],
          fill = data[, groupvar])
    ) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = plot_title, subtitle = plot_subtitle,
         x = x_axis_title, y = y_axis_title, caption = plot_caption) +
    scale_fill_manual(name = legend_title,
                      labels = group_labels,
                      values = color_scheme) +
    theme(axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    theme_settings

  return(out)
}


# input: 'data' is a matrix
# Function: create side by side violin plots for the columns in 'data'
# return: ggplot object
makeViolinPlot <- function(data,
                           variable_name,
                           plot_title, plot_subtitle,
                           x_axis_title = NULL,
                           y_axis_title = NULL,
                           y_axis_limits = NULL,
                           color_scheme
) {

  # Convert data to long form
  plot_data <- tidyr::pivot_longer(as.data.frame(data),
                                   cols = everything(),
                                   names_to = 'x_variable',
                                   values_to = variable_name)
  plot_data$x_variable <- factor(plot_data$x_variable, levels = colnames(data))

  plot <- ggplot(
    as.data.frame(plot_data),
    aes(y = unlist(plot_data[, variable_name]),
        x = unlist(plot_data[, "x_variable"]),
        fill = unlist(plot_data[, "x_variable"]))) +
    geom_violin(width = 1) +
    geom_boxplot(width = 0.05, color = "gray60",
                 alpha = 0.5, outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE, option = color_scheme) +
    ylim(y_axis_limits) +
    labs(title = plot_title, subtitle = plot_subtitle,
         y = y_axis_title, x = x_axis_title)

  return(plot)

}

# Function: construct multiple side-by-side violin plots using 'data',
#  where each side-by-side plot corresponds to a specific 2D slice of 'data';
# stack the plots vertically in a compound plot
# input 'data' is an array whose third dimension indexes observations
# input 'slice' specifies whether to use row slices or column slices
# input 'plot_titles' is a character vector whose length matches the dimension
#  of 'data' specified in 'slice' (one title per slice, i.e. per plot)
# input 'plot_subtitles' is analogous to 'plot_titles', but for plot subtitles
# other inputs are passed to makeViolinPlot()
makeViolinPlots <- function(data,
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings))
}






# Themes ------------------------------------------------------------------

# Create a theme for use with a ggplot
makeTheme <- function(
  base_theme = theme_minimal,
  base_size = 11,
  show_legend_title = TRUE,
  legend_position = "none",
  legend_margin = margin(),
  legend_title_size = 7,
  legend_text_size = 7,
  legend_symbol_size = 12,
  legend_symbol_spacing = 0,
  fontsize_x = 7,
  fontsize_y = 7,
  fontsize_title = 10,
  fontsize_subtitle = 9,
  fontsize_x_axis = 8,
  fontsize_y_axis = 8,
  plot_margin = margin(5, 5, 5, 5),
  aspect = 1,
  title_position = "panel"
) {
  out <-
    base_theme(
      base_size = base_size
    ) +
    theme(
      plot.margin = plot_margin,
      aspect.ratio = aspect,
      axis.text.x = element_text(size = fontsize_x),
      axis.title.x = element_text(size = fontsize_x_axis),
      axis.text.y = element_text(size = fontsize_y),
      axis.title.y = element_text(size = fontsize_y_axis),
      plot.subtitle = element_text(size = fontsize_subtitle),
      plot.title = element_text(size = fontsize_title,
                                face = 'bold'),
      legend.position = legend_position,
      legend.key.size = unit(legend_symbol_size, "pt"), # legend symbols
      legend.text = element_text(size = legend_text_size),
      legend.spacing = unit(legend_symbol_spacing, "pt"),
      legend.margin = legend_margin,
      legend.title = element_text(size = legend_title_size,
                                  color = "grey30"),
      plot.title.position = title_position)
  if (!show_legend_title) {
    out <- out + theme(legend.title = element_blank())
  }

  return(out)
}


theme_violin <- function(base_size = 11,
                         fontsize_x = rel(1),
                         fontsize_y = rel(0.9),
                         fontsize_title = rel(1.2),
                         fontsize_subtitle = rel(0.9),
                         fontsize_x_axis = rel(0.9),
                         fontsize_y_axis = rel(1),
                         plot_margin = margin(l = 5, r = 5),
                         aspect = 0.8,
                         x_axis_margin = 5) {
  theme_minimal() +
    theme(aspect.ratio = aspect, plot.margin = plot_margin,
          legend.position = "none",
          axis.title.x = element_text(size = fontsize_x_axis,
                                      margin = margin(t = x_axis_margin,
                                                      unit = "pt")),
          axis.text.x = element_text(size = fontsize_x),
          axis.text.y = element_text(size = fontsize_y),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(size = fontsize_subtitle),
          plot.title = element_text(size = fontsize_title, face = 'bold'))
}

theme_lineplot <- function(base_theme = theme_minimal,
                           base_size = 11,
                           show_legend_title = TRUE,
                           legend_position = "none",
                           legend_margins = margin(),
                           legend_title_size = 7,
                           legend_text_size = 7,
                           legend_symbol_size = 12,
                           legend_symbol_spacing = 0,
                           fontsize_x = 7,
                           fontsize_y = 7,
                           fontsize_title = 10,
                           fontsize_subtitle = 9,
                           fontsize_x_axis = 8,
                           fontsize_y_axis = 8) {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis)
}



# Titles & Subtitles ------------------------------------------------------

# Create list of settings for the joint title of a compound plot
makeTitleSettings <- function(joint_title = "Title",
                              joint_subtitle = "Subtitle",
                              joint_title_size = 20,
                              joint_subtitle_size = 13,
                              joint_title_height = 0.1,
                              title_overlay_margins = margin(t = 7, l = 7),
                              joint_subtitle_height = 0.1,
                              subtitle_overlay_margins = margin(),
                              joint_legend_width = 0.2,
                              joint_legend_height = 0.1,
                              joint_legend_margins = margin()) {
  list("joint_title" = joint_title,
       "joint_subtitle" = joint_subtitle,
       "joint_title_size" = joint_title_size,
       "joint_subtitle_size" = joint_subtitle_size,
       "joint_title_height" = joint_title_height,
       "joint_subtitle_height" = joint_subtitle_height,
       "title_overlay_margins" = title_overlay_margins,
       "subtitle_overlay_margins" = subtitle_overlay_margins,
       "joint_legend_width" = joint_legend_width,
       "joint_legend_height" = joint_legend_height,
       "joint_legend_margins" = joint_legend_margins)
}

overlayJointPlotElements <- function(joint_plot, title_settings, theme_settings,
                                     legend_from = NULL # no legend by default
) {
  if (!is.null(legend_from)) { # include legend
    if (theme_settings$legend.position == "bottom") {
      # Extract legend from an individual plot
      joint_legend <- cowplot::get_legend(
        legend_from +
          theme(legend.box.margin = title_settings$joint_legend_margins))
      # Overlay title, subtitle and legend onto joint plot and return
      out <- cowplot::plot_grid(
        makePlotTitle(title_settings),
        makePlotSubtitle(title_settings),
        joint_plot,
        joint_legend,
        ncol = 1,
        rel_heights = c(
          title_settings$joint_title_height,
          title_settings$joint_subtitle_height,
          1,
          title_settings$joint_legend_height))
      return(out)
    } else if (theme_settings$legend.position == "top") {
      # Extract legend from an individual plot
      joint_legend <- cowplot::get_legend(
        legend_from +
          theme(legend.box.margin = title_settings$joint_legend_margins))
      # Overlay title, subtitle and legend onto joint plot and return
      out <- cowplot::plot_grid(
        makePlotTitle(title_settings),
        makePlotSubtitle(title_settings),
        joint_legend,
        joint_plot,
        ncol = 1,
        rel_heights = c(
          title_settings$joint_title_height,
          title_settings$joint_subtitle_height,
          title_settings$joint_legend_height,
          1))
      return(out)
    } else if (theme_settings$legend.position == "right") {
      # Extract legend from an individual plot and add to joint plot
      joint_legend <- cowplot::get_legend(
        legend_from +
          theme(legend.box.margin = title_settings$joint_legend_margins))
      joint_plot_w_legend <- cowplot::plot_grid(
        joint_plot, joint_legend,
        ncol = 2,
        rel_widths = c(1, title_settings$joint_legend_width))
      # Overlay title, subtitle onto joint plot and return
      out <- cowplot::plot_grid(
        makePlotTitle(title_settings),
        makePlotSubtitle(title_settings),
        joint_plot_w_legend,
        ncol = 1,
        rel_heights = c(
          title_settings$joint_title_height,
          title_settings$joint_subtitle_height,
          1))
      return(out)
    } else if (theme_settings$legend.position == "left") {
      # Extract legend from an individual plot and add to joint plot
      joint_legend <- cowplot::get_legend(
        legend_from +
          theme(legend.box.margin = title_settings$joint_legend_margins))
      joint_plot_w_legend <- cowplot::plot_grid(
        joint_legend, joint_plot,
        ncol = 2,
        rel_widths = c(title_settings$joint_legend_width, 1))
      # Overlay title, subtitle onto joint plot and return
      out <- cowplot::plot_grid(
        makePlotTitle(title_settings),
        makePlotSubtitle(title_settings),
        joint_plot_w_legend,
        ncol = 1,
        rel_heights = c(
          title_settings$joint_title_height,
          title_settings$joint_subtitle_height,
          1))
      return(out)
    } else { # no valid legend position
      # Overlay title, subtitle onto joint plot and return
      out <- cowplot::plot_grid(
        makePlotTitle(title_settings),
        makePlotSubtitle(title_settings),
        joint_plot,
        ncol = 1,
        rel_heights = c(
          title_settings$joint_title_height,
          title_settings$joint_subtitle_height,
          1))
      return(out)
    }
  } else { # no legend
    # Overlay title, subtitle onto joint plot and return
    out <- cowplot::plot_grid(
      makePlotTitle(title_settings),
      makePlotSubtitle(title_settings),
      joint_plot,
      ncol = 1,
      rel_heights = c(
        title_settings$joint_title_height,
        title_settings$joint_subtitle_height,
        1))
    return(out)
  }
}

# Create multi-plot title for use with cowplot::plot_grid()
# title_settings is a list created with makeTitleSettings()
makePlotTitle <- function(title_settings = makeTitleSettings()) {
  ggdraw() +
    draw_label(title_settings$joint_title,
               size = title_settings$joint_title_size,
               fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = title_settings$title_overlay_margins)
}


# Create multi-plot title for use with cowplot::plot_grid()
# title_settings is a list created with makeTitleSettings()
makePlotSubtitle <- function(title_settings = makeTitleSettings()) {
  ggdraw() +
    draw_label(title_settings$joint_subtitle,
               size = title_settings$joint_subtitle_size,
               x = 0, hjust = 0) +
    theme(plot.margin = title_settings$subtitle_overlay_margins)
}

# scenario text description for plot subtitles
makeScenarioDesc <- function() {

  if (x_type == 'cts') feature_type <- 'Continuous features'
  if (x_type == 'snp') feature_type <- 'Simulated SNP set'
  if (error_distribution == 'cauchy') { err_dist <- 'Cauchy' }
  if (error_distribution == 'normal') { err_dist <- 'normal' }
  if (x_type == 'cts') num_signals <-
      switch(signal_density, 'sparse' = 7L, 'dense' = 80L)
  if (x_type == 'snp') num_signals <-
      switch(signal_density, 'sparse' = 28L, 'dense' = 122L)
  signal_label <- paste0(num_signals, "-variable signal set"
                         # " at ", 100 * signal_strength, '% strength',
  )
  if (x_type == 'snp') {
    if (signal_correlation == 'high') signal_label <-
        paste0(signal_label, ' with strongly-correlated components')
    if (signal_correlation == 'low') signal_label <-
        paste0(signal_label, ' with weakly-correlated components')
  }
  if (error_correlation_strength == 0) {
    error_corr_label <- 'errors with uncorrelated components'
  } else { # \u00B1 for plus/minus
    error_corr_label <-
      paste0('errors with correlated components (\u00B1', '0.5 pairwise)')
  }
  scenario_desc <- paste0(
    feature_type, ', ', signal_label, ', ', 'p = ', p, ', n = ', n,
    '\n', 'Multivariate ', err_dist, ' ', error_corr_label)
  return(scenario_desc)
}

title_violin <- function(joint_title = "Title",
                         joint_subtitle = "Subtitle",
                         joint_title_size = 20,
                         joint_subtitle_size = 13,
                         joint_title_height = 0.13,
                         title_overlay_margins = margin(t = 10, l = 7)) {
  makeTitleSettings(joint_title, joint_subtitle, joint_title_size,
                    joint_subtitle_size, joint_title_height,
                    title_overlay_margins)
}


# Colors & Shapes ---------------------------------------------------------

# Viridis color palettes with custom-set default values
colors_plasma <- function(n_colors, direction = -1, begin = 0, end = 0.85) {
  viridis::plasma(n_colors, begin = begin, end = end, direction = direction)
}
colors_inferno <- function(n_colors, direction = 1, begin = 0.15, end = 0.85) {
  viridis::inferno(n_colors, begin = begin, end = end, direction = direction)
}
colors_magma <- function(n_colors, direction = 1, begin = 0.15, end = 0.85) {
  viridis::magma(n_colors, begin = begin, end = end, direction = direction)
}
colors_viridis <- function(n_colors, direction = 1, begin = 0, end = 1) {
  viridis::viridis(n_colors, begin = begin, end = end, direction = direction)
}
colors_turbo <- function(n_colors, direction = -1, begin = 0, end = 1) {
  viridis::turbo(n_colors, begin = begin, end = end, direction = direction)
}

six_colors <- c("magenta2",
                "purple3",
                "blue",
                # "dodgerblue2",
                # "deepskyblue2",
                # "cyan",
                "aquamarine",
                # "green4",
                "green3",
                # "chartreuse",
                # "gold",
                # "goldenrod",
                "darkorange1")
all_colors <- c("magenta2",
                "purple3",
                "blue",
                "dodgerblue2",
                "deepskyblue2",
                "aquamarine",
                "cyan",
                "green4",
                "green3",
                "chartreuse",
                "gold",
                "goldenrod",
                "darkorange1")

# point shapes for line plots
all_shapes <- c(15, # solid square
                19, # solid circle
                17, # solid triangle
                18, # solid diamond
                0, # square outline
                5, # diamond outline
                2, # triangle outline
                6, # lower triangle outline
                1, # circle outline
                4, # X
                3,  # cross
                20, # solid small circle
                7, 8, 9, 10, 11, 12, 13, 14, 25, 21, 22, 23, 24
)

# Power -------------------------------------------------------------------

powerPlotFiles <- function() {

  # Filename stem describing set of scenarios for current plots
  part1 <- paste0('m', num_replicates, '_', x_type, '_',
                  error_distribution, '_s', signal_strength * 100)
  if (x_type == 'snp') {
    part2 <- paste0('_sc-', signal_correlation)
  } else { part2 <- character() }
  stem <- paste0(part1, part2, '_p', p)

  # file containing plot data
  file_plotdata <- file.path(dir_plotdata_power, paste0(stem, '.Rdata'))

  # files for plots
  file_mainplot <- file.path(dir_plots_power, paste0(stem, '.pdf'))
  file_Qplot <- file.path(dir_plots_power, paste0(stem, '_Q.pdf'))
  file_Qbarplot <- file.path(dir_plots_power, paste0(stem, '_Qbar.pdf'))

  return(list("plotdata" = file_plotdata,
              "main" = file_mainplot,
              "qplot" = file_Qplot,
              "qbarplot" = file_Qbarplot,
              "files_exist" =
                file.exists(file_mainplot) &
                file.exists(file_Qplot) &
                file.exists(file_Qbarplot)))
}

makePowerPlotDesc <- function() {

  if (x_type == 'cts') feature_type <- 'Continuous features'
  if (x_type == 'snp') feature_type <- 'Simulated SNP set'
  plot_desc <- paste0("\n", feature_type, ' (p = ', p, ')')

  if (x_type == 'snp') {
    if (signal_correlation == 'high') plot_desc <-
        paste0(plot_desc, ' with strongly-correlated signal variables')
    if (signal_correlation == 'low') plot_desc <-
        paste0(plot_desc, ' with weakly-correlated signal variables')
  }

  # if (error_distribution == 'cauchy') { err_dist <- 'Cauchy' }
  # if (error_distribution == 'normal') { err_dist <- 'normal' }
  # plot_desc <- paste0(plot_desc, '\nMultivariate ', err_dist, ' errors'))

  plot_desc <-  paste0(
    plot_desc,
    # '\n', 100 * signal_strength, '% signal strength',
    '\nEach value based on ', num_replicates, ' simulated data replicates; ',
    'AMKAT P-values estimated using ', num_permutations, ' permutations')

  return(plot_desc)
}

makeCompoundPowerPlot <- function(
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings,
                                  legend_from = plot_sparse_corr1))

}

makeCompoundPowerBarPlot <- function(
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings,
                                  legend_from = plot_sparse_corr1))
}

makePowerPlot <- function(
  plot_data, corr_values, corr_labels, corr_index, num_signals, legend_title,
  color_scheme, shape_scheme, line_size, point_size, y_scale_max,
  theme_settings, x_breaks) {
  makeLinePlot(
    data = plot_data[
      which(
        (plot_data$corr == corr_values[[corr_index]]) &
          (plot_data$signals == num_signals)), ],
    xvar = "sample_size", yvar = "value", groupvar = "method",
    color_scheme = color_scheme, shape_scheme = shape_scheme,
    plot_title = paste0(num_signals, '-variable signal set'),
    plot_subtitle = corr_labels[[corr_index]],
    plot_caption = NULL,
    x_axis_title = "Sample Size", y_axis_title = "Power",
    legend_title, line_size, point_size, theme_settings
  ) +
    scale_y_continuous(breaks = seq(from = 0, to = y_scale_max, by = 0.1),
                       limits = c(0, y_scale_max)) +
    scale_x_continuous(breaks = x_breaks)
}

makePowerBarPlot <- function(
  plot_data, corr_values, corr_labels, corr_index, num_signals, legend_title,
  color_scheme, group_labels, y_scale_max, theme_settings, x_breaks) {

  makeBarPlot(
    data = plot_data[
      which(
        (plot_data$corr == corr_values[[corr_index]]) &
          (plot_data$signals == num_signals)), ],
    xvar = "sample_size", yvar = "value", groupvar = "method", group_labels,
    color_scheme,
    plot_title = paste0(num_signals, '-variable signal set'),
    plot_subtitle = corr_labels[[corr_index]],
    plot_caption = NULL,
    x_axis_title = "Sample Size", y_axis_title = "Power",
    legend_title, theme_settings
  ) +
    scale_y_continuous(breaks = seq(from = 0, to = y_scale_max, by = 0.1),
                       limits = c(0, y_scale_max), expand = c(0, 0)) +
    scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)
}


# Adaptive Across ---------------------------------------------------------

# Generate list of file paths for plot data and plots
adaptiveAcrossPlotFiles <- function() {

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
          file.path(dir_plots_kernel_selection,
                    paste0(kerplot_filestem, '_', y_col_labels, '.pdf')) })
  }
  if (x_type == "snp") {
    # vector of file paths for kernel selection plots
    kerplot_files <- file.path(dir_plots_kernel_selection,
                               paste0(stem, '_', y_col_labels, '.pdf'))
  }
  phimrplot_retention_file <-
    file.path(dir_plots_feature_selection, paste0(stem, '_keeprates.pdf'))

  phimrplot_density_file <-
    file.path(dir_plots_feature_selection, paste0(stem, '_density.pdf'))

  return(list("plotdata" = plotdata_file,
              "kerplots" = kerplot_files, # character matrix/vector
              "phimrplot_retention" = phimrplot_retention_file,
              "phimrplot_density" = phimrplot_density_file,
              "kerplots_exist" =
                sum(file.exists(kerplot_files)) == length(kerplot_files)))
}


makeKerSelPlots <- function(
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
          'Correlated error components (', rho, ' = 0.5)'))) +
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
          'Correlated error components (', rho, ' = 0.5)'))) +
      ylab(NULL)
    joint_plot <- cowplot::plot_grid(
      plot11 + theme(legend.position = "none"),
      plot12 + theme(legend.position = "none"),
      plot21 + theme(legend.position = "none"),
      plot22 + theme(legend.position = "none"),
      ncol = 2, labels = rep("", 4), align = "vh")
  }
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings,
                                  legend_from = plot11))

}

makeKerSelPlot <- function(
  data, rho, n_signals, p, color_scheme, shape_scheme, line_size,
  point_size, theme_settings, x_breaks,
  title = paste0(n_signals, '-variable signal set'),
  subtitle = paste0('p = ', p)) {

  makeLinePlot(
    data[which(
      data$rho == rho & data$signals == n_signals & data$featdim == p), ],
    xvar = "samplesize", yvar = "value", groupvar = "kernel",
    color_scheme = color_scheme, shape_scheme = shape_scheme,
    plot_title = title, plot_subtitle = subtitle, plot_caption = NULL,
    x_axis_title = "Sample Size", y_axis_title = "Selection Rate",
    legend_title = "kernel", line_size, point_size, theme_settings
  ) +
    ylim(0, 1) +
    scale_x_continuous(breaks = x_breaks)
}

makePhimrRetentionPlots <- function(data) {

  data <- data[plotdata_phimr$datatype %in% c('signal', 'noise'), ]
  data$linegroup <- paste0(data$datatype, data$featdim)
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$rho))
  sample_sizes <- sort(unique(data$samplesize))
  plot_sparse_corr1 <- phimrRetentionPlot(
    data, corr_values[[1]], signal_values[[1]], sample_sizes)
  plot_dense_corr1 <- phimrRetentionPlot(
    data, corr_values[[1]], signal_values[[2]], sample_sizes)
  plot_sparse_corr2 <- phimrRetentionPlot(
    data, corr_values[[2]], signal_values[[1]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  plot_dense_corr2 <- phimrRetentionPlot(
    data, corr_values[[2]], signal_values[[2]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  joint_plot <- cowplot::plot_grid(
    plot_sparse_corr1 + theme(legend.position = "none"),
    plot_sparse_corr2 + theme(legend.position = "none"),
    plot_dense_corr1 + theme(legend.position = "none"),
    plot_dense_corr2 + theme(legend.position = "none"),
    ncol = 2, labels = rep("", 4), align = "vh")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements(
    joint_plot, title_settings = title_phimr(),
    theme_settings = theme_phimr(), legend_from = plot_sparse_corr1))
}

phimrRetentionPlot <- function(data, corr_value, num_signals, x_breaks) {
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
    theme_settings = theme_phimr(),
    color_by = "featdim", shapes_by = "datatype",
    color_legend_title = "feature dim.", shape_legend_title = "variable type"
  ) +
    scale_y_continuous(breaks = seq(from = 0, to = 1,
                                    by = 0.1),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = x_breaks)
}


makePhimrDensityPlots <- function(data) {

  data <- data[plotdata_phimr$datatype == 'density', ]
  signal_values <- sort(unique(data$signals))
  corr_values <- sort(unique(data$rho))
  sample_sizes <- sort(unique(data$samplesize))
  plot_sparse_corr1 <- phimrDensityPlot(
    data, corr_values[[1]], signal_values[[1]], sample_sizes)
  plot_dense_corr1 <- phimrDensityPlot(
    data, corr_values[[1]], signal_values[[2]], sample_sizes)
  plot_sparse_corr2 <- phimrDensityPlot(
    data, corr_values[[2]], signal_values[[1]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  plot_dense_corr2 <- phimrDensityPlot(
    data, corr_values[[2]], signal_values[[2]], sample_sizes) +
    labs(title = NULL) + ylab(NULL)
  joint_plot <- cowplot::plot_grid(
    plot_sparse_corr1 + theme(legend.position = "none"),
    plot_sparse_corr2 + theme(legend.position = "none"),
    plot_dense_corr1 + theme(legend.position = "none") ,
    plot_dense_corr2 + theme(legend.position = "none"),
    ncol = 2, labels = rep("", 4), align = "vh")
  # Overlay title, subtitle and legend onto joint plot and return
  return(overlayJointPlotElements(
    joint_plot,
    title_settings = title_phimr(
      joint_title = "PhiMr Filter Effect on Signal Density",
      joint_subtitle = makePhimrSubtitle("density")),
    theme_settings = theme_phimr(), legend_from = plot_sparse_corr1))
}

phimrDensityPlot <- function(data, corr_value, num_signals, x_breaks) {
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
    theme_settings = theme_phimr()
  ) +
    scale_y_continuous(breaks = seq(from = 1, to = 2,
                                    by = 0.1),
                       limits = c(1, 2)) +
    scale_x_continuous(breaks = x_breaks)
}
# Adaptive Within ---------------------------------------------------------

# Generate list of file paths for plot data and plots
pvaluePlotFiles <- function() {

  source(file.path(dir_src, 'define_scenario_filename.R'))

  file_data_stats <-
    file.path(dir_data_adaptive_within,
              paste0(scenario_filename_stem, '_raw.Rdata'))
  file_data_pvalues <-
    file.path(dir_data_adaptive_within,
              paste0(scenario_filename_stem, '_pvalues.Rdata'))
  file_plot_statdistr <-
    file.path(dir_plots_adaptive_within,
              paste0(scenario_filename_stem, "_stat_distr.pdf"))
  file_plot_violinQ <-
    file.path(dir_plots_adaptive_within,
              paste0(scenario_filename_stem, "_violin_Q.pdf"))
  file_plot_violinB <-
    file.path(dir_plots_adaptive_within,
              paste0(scenario_filename_stem, "_violin_B.pdf"))
  file_plot_sdline <-
    file.path(dir_plots_adaptive_within,
              paste0(scenario_filename_stem, "_SDlineplot.pdf"))
  file_plot_meanline <-
    file.path(dir_plots_adaptive_within,
              paste0(scenario_filename_stem, "_meanlineplot.pdf"))
  file_plot_phimr <-
    file.path(dir_plots_adaptive_within,
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
makeStatDistrPlots <- function(
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings))
}

# Make single statistic distribution plot
makeStatDistrPlot <- function(plot_data, plot_title, y_min, y_max,
                              line_value, line_label, line_label_position,
                              plot_subtitle = NULL, annotation_size = 3,
                              theme_settings = theme_statDistr()) {
  out <- ggplot(plot_data,
                aes(x = group, y = value, fill = group)) +
    geom_violin(width = 1) +
    geom_boxplot(width = 0.05, color = "gray60", alpha = 0.5,
                 outlier.shape = NA) +
    geom_hline(yintercept = line_value, color = 'purple') +
    scale_y_continuous(limits = c(y_min, y_max)) +
    annotate('text', label = line_label, x = line_label_position,
             y = line_value, size = annotation_size, color = 'purple') +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         y = 'Statistic Value', x = NULL) +
    scale_fill_viridis(discrete = TRUE) +
    theme_settings +
    theme(panel.grid.major.x = element_blank())
  return(out)
}

# pvalues is an array of pvalues
makeViolinQPlots <- function(
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

  out <- makeViolinPlots(
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
makeViolinBPlots <- function(
  pvalues, group_Q_values = Q_values_violinB,
  theme_settings = theme_violinB(),
  title_settings = title_violinB(num_replicates),
  color_code = color_code_violinB) {

  # create temp copy of input data and rename columns
  all_Q_values <- dimnames(pvalues)[[2]]
  Q_indices <- c(1, which(all_Q_values %in% paste0("PF-", group_Q_values)))

  out <- makeViolinPlots(
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
makePVLinePlots <- function(pvalues,
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings))
}


# Input 3D Array 'data' and specify whether to slice by rows or columns
# Within each slice, compute the specified summary stat of the complementary dimension
#  (e.g., column SDs if slicing by row)
# Return 2D data frame with 3 columns
#  If slicing by row:
#  - 'x_variable' is the corresponding colname in 'data'
#  - 'stat' is the summary stat of the corresponding col in 'data'
#  - 'group' is the corresponding rowname in 'data'
#  If slicing by col:
#  - 'x_variable' is the corresponding rowname in 'data'
#  - 'stat' is the summary stat of the corresponding row in 'data'
#  - 'group' is the corresponding colname in 'data'
preparePVLinePlotData <- function(data,
                                  slice = "rows",
                                  summary_stat = sd) {

  # initialize output
  out <- matrix(nrow = 0, ncol = 3)

  # If slicing by row:
  if (slice %in% c("r", "row", "rows")) {

    for (row_ind in 1:dim(data)[[1]]) { # iterate over the rows of the array

      # extract current row slice as 2D matrix;
      #  rows of array become matrix columns;
      #  3rd dim of array becomes matrix rows
      current_slice <- t(data[row_ind, , ])

      # compute summary stats by column
      summaryvec <- apply(current_slice, MARGIN = 2, FUN = summary_stat)
      summaryrow <- matrix(summaryvec, nrow = 1, ncol = length(summaryvec))
      colnames(summaryrow) <- names(summaryvec)

      # convert sdvec to long form
      rows_to_append <-
        tidyr::pivot_longer(
          data = as.data.frame(summaryrow),
          cols = everything(),
          names_to = 'x_variable',
          values_to = 'stat'
        )

      # add variable for slice label
      rows_to_append$group <- dimnames(data)[[1]][[row_ind]]

      # append long-form data to output
      out <- rbind(out, rows_to_append)

    }

    # convert to data frame
    out <- as.data.frame(out)
    out$x_variable[out$x_variable == "NF"] <- "PF-0" # change NF to PF-0
    out$x_variable <-
      sapply(out$x_variable, # strip prefix "PF-"
             function(x) { substr(x, start = 4, stop = nchar(x)) })
    out$x_variable <- as.numeric(out$x_variable)
    out$group <- factor(out$group, levels = dimnames(data)[[1]])

  }

  # If slicing by column:
  if (slice %in% c("c", "col", "column", "columns")) {

    for (col_ind in 1:dim(data)[[2]]) { # iterate over the cols of the array

      # extract current col slice as 2D matrix;
      #  3rd dim of array becomes matrix rows
      current_slice <- t(data[, col_ind, ])

      # compute summary stats by column
      summaryvec <- apply(current_slice, MARGIN = 2, FUN = summary_stat)
      summaryrow <- matrix(summaryvec, nrow = 1, ncol = length(summaryvec))
      colnames(summaryrow) <- names(summaryvec)

      # convert sdvec to long form
      rows_to_append <-
        tidyr::pivot_longer(
          data = as.data.frame(summaryrow),
          cols = everything(),
          names_to = 'x_variable',
          values_to = 'stat'
        )

      # add variable for slice label
      rows_to_append$group <- dimnames(data)[[2]][[col_ind]]

      # append long-form data to output
      out <- rbind(out, rows_to_append)

    }

    # convert to data frame
    out <- as.data.frame(out)
    out$x_variable <- as.numeric(out$x_variable)

    # Format group labels and convert to factor
    out$group[which(out$group == "NF")] <- "PF-0 (no filter)"
    out$group <-
      sapply(out$group, # strip prefix "PF-"
             function(x) { substr(x, start = 4, stop = nchar(x)) })

    sorted_unique_numeric_vals <-
      sort(unique(as.numeric(out$group[out$group != "0 (no filter)"])))
    group_levels <- c("0 (no filter)", as.character(sorted_unique_numeric_vals))
    out$group <- factor(out$group, levels = group_levels)

  }

  return(out)

}


# # input: 'data' is a matrix;
# #  'variable_name' is a character string containing the name of the variable on
# #    which 'data' contains recorded measurements (e.g., P-values)
# # Function: convert data to long form used by ggplot2;
# # return k x 3 matrix where k is the number of entries in the data frame 'data'
# # Each row in the new matrix corresponds to a value in 'data'
# # first column in plot_data records the value; column named by 'variable_name'
# # column 'group' in plot_data records column name in 'data' for the value
# #  column 'group_labels' is a factor whose levels encode the following info:
# #   - group name from 'group' column
# #   - sample SD for member group (if include_sd = TRUE)
# #   - null rejection rate for member group (if include_rejection = TRUE)
# prepareViolinPlotData <- function(data, variable_name,
#                                   include_rejection = FALSE, include_sd = FALSE) {
#
#   method_labels <- colnames(data)
#   plot_data <- tidyr::gather(as.data.frame(data),
#                              key = 'group', value = variable_name,
#                              factor_key = TRUE)
#
#   # Add SD values to plot data
#   if (include_sd | include_rejection) {
#     std_dev_vec <- list('group' = method_labels,
#                         'std_dev' = apply(data, MARGIN = 2,
#                                           function(x){round(sd(x), 4)}))
#     plot_data <- dplyr::left_join(plot_data, std_dev_vec, copy = TRUE,
#                                   by = 'group')
#   }
#
#   # Add null rejection rates to plot data
#   if (include_rejection) {
#     reject_rates <-
#       list('group' = method_labels,
#            'null_rejection_rate' = apply(data, MARGIN = 2,
#                                          function(x){mean(x < alpha)}))
#     plot_data <- dplyr::left_join(plot_data, reject_rates, copy = TRUE,
#                                   by = 'group')
#     group_labels <- paste0(reject_rates$group, '\n', size_or_power , ' = ',
#                            as.character(reject_rates$null_rejection_rate),
#                            '\n sd = ', std_dev_vec$std_dev)
#     plot_data <-
#       dplyr::mutate(
#         plot_data,
#         group_label =
#           factor(
#             paste0(group, '\n', size_or_power , ' = ',
#                    as.character(null_rejection_rate), '\n sd = ', std_dev),
#             levels = group_labels))
#   } else if (include_sd) {
#     group_labels <- paste0(std_dev_vec$group, '\n sd = ', std_dev_vec$std_dev)
#     plot_data <-
#       dplyr::mutate(plot_data,
#                     group_label = factor(paste0(group, '\n sd = ', std_dev),
#                                          levels = group_labels))
#   } else {
#     # Add a group_label column that duplicates the group column as a factor
#     plot_data <- dplyr::mutate(
#       plot_data, group_label = factor(group, levels = method_labels))
#   }
#   return(plot_data)
# }

makePhimrHistograms <- function(keeprates, signal_indices) {

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
  return(
    overlayJointPlotElements(
      joint_plot =
        cowplot::plot_grid(plot_signal, plot_noise,
                           ncol = 2, labels = c("", ""), align = "hv"),
      title_settings = title_phimr_histo(),
      theme_settings = theme_phimr_histo()))
}

makePhimrHistogram <- function(values, type, maxcount) {
  if (type == "signal") {
    border_color = "darkblue"
    fill_color = "deepskyblue2"
    title = "Signal Variables"
  }
  if (type == "noise") {
    border_color = "darkred"
    fill_color = "indianred1"
    title = "Noise Variables"
  }
  ggplot(data.frame(values = values),
         aes(x = values)) +
    geom_histogram(color = border_color, fill = fill_color,
                   binwidth = 0.05, boundary = 0) +
    theme_phimr_histo() +
    scale_x_continuous(limits = c(-0.05, 1),
                       breaks = seq(from = 0, to = 1, by = 0.05)) +
    scale_y_continuous(limits = c(0, maxcount)) +
    labs(x = "Retention Rate", y = "Number of Variables", title = title) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(margin = margin(b = 15)),
          axis.text.x = element_text(margin = margin(t = -7, b = 5))) +
    guides(x = guide_axis(angle = 90)) +
    geom_vline(xintercept = mean(values), color = border_color,
               linetype = "dashed", size = 1.2)
}


getSignalIndices <- function(x_type, signal_density) {
  # Case 1: X is continuous
  #   In this case, X exhibits a short-range correlation structure,
  #   with the correlation between component i and j equal to 0.6^(i-j).
  if (x_type == 'cts') {

    # Subcase A: Sparse signal set (7 signal variables)
    if (signal_density == 'sparse') {

      signal_indices_shared <- c(2, 4, 6, 8) # shared signals for all Y components
      signal_indices_y1 <- 3                 # additional signals for Y_1
      signal_indices_y2_only <- 5            # additional signals for Y_2
      signal_indices_y3 <- 7                 # additional signals for Y_3
      signal_indices_y4 <- 7                 # additional signals for Y_4

    }

    # Subcase B: Dense signal set (80 signal variables)
    if (signal_density == 'dense') {

      # shared signals for all Y components
      #   clustered in groups of 4 with a 4-variable gap between each group
      signal_indices_shared <- c((5:8), (13:16), (21:24), (29:32), (37:40),
                                 (45:48), (53:56), (61:64), (69:72), (77:80))

      # additional signals for Y_2
      #   clustered as with the shared components, but offset by 4 variables,
      #   so that the clusters for Y_2 occupy the gaps between the shared clusters
      signal_indices_y2_only <-
        c((3:4), (9:12), (17:20), (25:28), (33:36), (41:44), (49:52), (57:60),
          (65:68), (73:76), (81:82))

      # additional signals for Y_1
      #   subset of the additional signals for Y_2, located on cluster boundaries;
      #   half of the signals also affect Y_3; the other half also affect Y_4
      signal_indices_y1 <- c(28, 33, 36, 41, 44, 49, 52, 57, 60, 65)

      # additional signals for Y_3
      #   subset of the additional signals for Y_2, located on cluster boundaries;
      #   five of the signals also affect Y_1; the other ten also affect Y_4
      signal_indices_y3 <- c(28, 33, 36, 41, 44,
                             4, 9, 12, 17, 20, 25, 68, 73, 76, 81)

      # additional signals for Y_4
      #   subset of the additional signals for Y_2, located on cluster boundaries;
      #   five of the signals also affect Y_1; the other ten also affect Y_3
      signal_indices_y4 <- c(49, 52, 57, 60, 65,
                             4, 9, 12, 17, 20, 25, 68, 73, 76, 81)

    }

  }

  # Case 2: X is discrete SNP-set data
  if (x_type == 'snp') {

    ### BASE ORIGINATING SET FOR SIGNAL VARIABLES ###

    # We begin by defining three regions of the reference SNP-set data that
    #  exhibit relatively low correlation among components
    region1 <- 132:(132 + 46)
    region2 <- 213:(213 + 30)
    region3 <- 505:(505 + 44)
    # Sparse signal set (28 signals)
    if (signal_density == 'sparse') {
      if (signal_correlation == 'high') {
        # Use the first 28 components of X as the base signal set
        signal_set <- 1:28
      } else {
        # Select signal variables from among the low-correlation regions
        signal_set <- c(region1[c(39, 41:46)],
                        region2[c(2:8)],
                        region3[c(9, 10, 11, 13, 15, 16, 17, 36:40, 42, 43)])
      }
    }
    # Dense signal set (122 signals)
    # Note: originating set will have 123 elements; only 122 will be used
    # for the actual signal set
    if (signal_density == 'dense') {
      if (signal_correlation == 'high') {
        # Use the first 123 components of X as the base signal set
        signal_set <- 1:123
      } else {
        # Use the entirety of the three low-corr regions as the signal set
        signal_set <- c(region1, region2, region3)
      }
    }

    ### Indices of Signal Set Used By Each Effect Function ###

    # Descriptions of spatial patterns of indices pertain to their distribution
    # within the signal set; note that the signal set may not correspond to a
    # contiguous region of X

    # shared signals for all Y components
    #   clustered in groups of 2 with a 2-variable gap between each group
    signal_indices_shared <-
      signal_set[sort(union(seq(from = 1, to = length(signal_set), by = 4),
                            seq(from = 2, to = length(signal_set), by = 4)))]

    # additional signals for Y_2
    #   clustered as with the shared components, but offset by 4 variables,
    #   so that the clusters for Y_2 occupy the gaps between the shared clusters
    signal_indices_y2_only <-
      signal_set[sort(union(seq(from = 3, to = length(signal_set), by = 4),
                            seq(from = 4, to = length(signal_set), by = 4)))]

    # additional signals for Y_1
    #   subset of the additional signals for Y_2, located in left of each cluster
    signal_indices_y1 <-
      signal_set[seq(from = 3, to = length(signal_set), by = 4)]

    # additional signals for Y_3
    #   subset of the additional signals for Y_2, located in right of each cluster
    signal_indices_y3 <-
      signal_set[seq(from = 4, to = length(signal_set), by = 4)]

    # additional signals for Y_4
    #   same as the additional signals for Y_2
    signal_indices_y4 <- signal_indices_y2_only

    # ensure that lengths of shared signal set and set of remaining signals match
    if (length(signal_indices_shared) > length(signal_indices_y2_only)) {
      signal_indices_shared <-
        signal_indices_shared[c(1:length(signal_indices_y2_only))]
    }
    if (length(signal_indices_y2_only) > length(signal_indices_shared)) {
      signal_indices_y2_only <-
        signal_indices_y2_only[c(1:length(signal_indices_shared))]
    }
  }

  # all signals affecting Y_1 (including shared signals)
  signal_indices_y1 <- union(signal_indices_shared, signal_indices_y1)

  # all signals affecting Y_3 (including shared signals)
  signal_indices_y3 <- union(signal_indices_shared, signal_indices_y3)

  # all signals affecting Y_4 (including shared signals)
  signal_indices_y4 <- union(signal_indices_shared, signal_indices_y4)

  signal_indices <-
    unique(c(signal_indices_shared, signal_indices_y1,
             signal_indices_y2_only, signal_indices_y3,
             signal_indices_y4))

  return(signal_indices)
}


# Runtime Benchmarking ----------------------------------------------------


makeBenchPlot <- function(data, title,
                          y_limits = NULL,
                          y_breaks = ggplot2::waiver(),
                          timefactor = 1e+6, # convert from ns (default to ms)
                          subtitle = NULL,
                          line_size = 0.4,
                          point_size = 2,
                          y_axis_title = "Median Execution Time (ms)",
                          theme_settings = makeTheme(),
                          color_scheme = colors_bench(length(rownames(data))),
                          shape_scheme = all_shapes
) {

  #convert data to long form with extra variable encoding row name
  longdata <- matrix(nrow = 0, ncol = 3) # initialize
  for (j in 1:ncol(data)) {
    for (i in 1:nrow(data)) {
      newrow <- c(data[i, j] / timefactor, # runtime
                  colnames(data)[[j]], # dimension
                  rownames(data)[[i]] # sample size
      )
      longdata <- rbind(longdata, newrow)
    }
  }
  colnames(longdata) <- c("y_variable", # runtime
                          "x_variable", # dimension
                          "group")  # sample size
  longdata <- as.data.frame(longdata)
  longdata$x_variable <- as.numeric(longdata$x_variable)
  longdata$y_variable <- as.numeric(longdata$y_variable)
  longdata$group <- as.factor(as.numeric(longdata$group))

  out <-
    makeLinePlot(
      longdata, xvar = "x_variable", yvar = "y_variable", groupvar = "group",
      color_scheme = color_scheme, shape_scheme = shape_scheme,
      plot_title = title, plot_subtitle = subtitle,
      x_axis_title = "Feature Dimension", y_axis_title = y_axis_title,
      legend_title = "sample size",
      line_size = line_size, point_size = point_size,
      theme_settings = theme_settings) +
    scale_x_continuous(trans = "log2",
                       breaks = sort(unique(longdata$x_variable)),
                       minor_breaks = NULL) +
    scale_y_continuous(trans = "log2", breaks = y_breaks, limits = y_limits,
                       minor_breaks = NULL) +
    guides(x = guide_axis(angle = 45))
  return(out)
}

# Data is an array; 3rd dim indexes the plots; rows index n, cols index p
makeKernelBenchPlots <- function(data = data_kermat,
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings,
                                  legend_from = kerplot_l))
}

# Data is a matrix; rows index n, cols index p
makeGenericBenchPlot <- function(data, num_executions,
                                 timefactor,
                                 y_axis_title,
                                 title,
                                 subtitle,
                                 line_size = line_size_bench_phimr,
                                 point_size = point_size_bench_phimr,
                                 theme_settings = theme_bench_phimr(),
                                 color_scheme =
                                   colors_bench(n_colors = nrow(data)),
                                 shape_scheme = all_shapes) {

  global_min <- 2^floor(log2(min(data) / timefactor))
  global_max <- 2^ceiling(log2(max(data) / timefactor))
  y_ticks <- 2^(0:log2(global_max))
  ylims <- c(global_min, global_max)

  out <- makeBenchPlot(
    data, title = title,
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    subtitle = subtitle,
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme)
  return(out)
}

# Data is a matrix; rows index n, cols index p
makePhimrBenchPlot <- function(data = data_phimr, num_executions = reps_phimr,
                               timefactor = timefactor_phimr,
                               y_axis_title = timelabel_phimr,
                               line_size = line_size_bench_phimr,
                               point_size = point_size_bench_phimr,
                               theme_settings = theme_bench_phimr(),
                               color_scheme =
                                 colors_bench(n_colors = nrow(data)),
                               shape_scheme = all_shapes) {

  global_min <- 2^floor(log2(min(data) / timefactor))
  global_max <- 2^ceiling(log2(max(data) / timefactor))
  y_ticks <- 2^(0:log2(global_max))
  ylims <- c(global_min, global_max)

  out <- makeBenchPlot(
    data, title = "PhiMr Filter Execution Time",
    y_limits = ylims, y_breaks = y_ticks, timefactor = timefactor,
    subtitle = paste0(
      "4 response variables; continuous features\nMedian time over ",
      num_executions, " executions of the phimr() function\nfrom the AMKAT R package"),
    line_size = line_size, point_size = point_size,
    y_axis_title = y_axis_title, theme_settings = theme_settings,
    color_scheme = color_scheme, shape_scheme = shape_scheme)
  return(out)
}


# Data is an array; 3rd dim indexes the plots; rows index n, cols index p
makeAmkatBenchPlots <- function(data = data_amkat,
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
  return(overlayJointPlotElements(joint_plot, title_settings, theme_settings,
                                  legend_from = plot_l))
}
