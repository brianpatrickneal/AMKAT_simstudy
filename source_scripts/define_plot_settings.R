
# Power -------------------------------------------------------------------


# Line and point sizes for line plots
line_size_power <- 0.6
point_size_power <- 2.5

# color scheme for main plot
colors_power <- function(n_colors) {
  colors_plasma(n_colors) # default: begin = 0, end = 0.85, direction = -1
}

# color scheme for AMKAT methods plot
colors_power_amkat <- function(n_colors =
                                 length(levels(plot_data_amkat$method))) {
  colors_inferno(n_colors) #default: begin = 0.15, end = 0.85, direction = 1
}

colors_powerbar_amkat <- function(n_colors) {
  colors_inferno(n_colors) #default: begin = 0.15, end = 0.85, direction = 1
}

# point shapes for line plots (used for main plot only)
shapes_power <- c(
  15, # solid square             OMGA
  20, # solid small circle,      DKAT
  # 17, # solid triangle
  18, # solid diamond            DKAT-LPK
  1, # circle outline            AMKAT NF
  3, # cross                     AMKAT PhiMr64
  4, # X                         AMKAT PhiMr128
  6, # lower triangle outline
  0, # square outline
  2, # triangle outline
  5 # diamond outline
  # 16, # solid circle
)
shapes_power_amkat <- c(
  15, # solid square             NF
  20, # solid small circle,      1
  17, # solid triangle           2
  18, # solid diamond            4
  0, # square outline            8
  1, # circle outline            16
  6, # lower triangle outline    32
  4, # X                         64
  3, # cross                     128
  2, # triangle outline
  5, # diamond outline
  16  # solid circle
)

theme_power <- function(
  base_theme = theme_minimal,
  base_size = 11,
  show_legend_title = FALSE,
  legend_position = "right",
  legend_margins = margin(l = 10),
  legend_title_size = rel(1),
  legend_text_size = rel(0.8),
  legend_symbol_size = 12,
  legend_symbol_spacing = 10,
  fontsize_x = rel(0.9),
  fontsize_y = rel(0.9),
  fontsize_title = rel(1.4),
  fontsize_subtitle = rel(0.9),
  fontsize_x_axis = rel(0.9),
  fontsize_y_axis = rel(1.2),
  plot_margin = margin(b = 10),
  aspect = 0.8,
  title_position = "panel") {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect,
    title_position)
}

title_power <- function(
  joint_title =
    "Simulated Power: AMKAT vs. Comparison Methods",
  joint_subtitle = makePowerPlotDesc(),
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = powerTitleHeight(length(unique(data$corr))),
  title_overlay_margins = margin(t = 0, l = 7),
  joint_subtitle_height = powerSubtitleHeight(length(unique(data$corr))),
  subtitle_overlay_margins = margin(t = -30, b = 10, l = 7),
  joint_legend_width = 0.25,
  joint_legend_height = 0.005,
  joint_legend_margins = margin(l = -10, r = 10)) {
  makeTitleSettings(
    joint_title, joint_subtitle,
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins,
    joint_legend_width, joint_legend_height, joint_legend_margins)
}
# Joint title height based on number of rows
powerTitleHeight <- function(n_rows) {
  switch(as.character(n_rows), "1" = 0.16, "2" = 0.07)
}
# Joint title height based on number of rows
powerSubtitleHeight <- function(n_rows) {
  switch(as.character(n_rows), "1" = 0.16, "2" = 0.11)
}

# Joint plot size based on number of rows
height_power <- function(n_rows) {
  switch(as.character(n_rows), "1" = 4, "2" = 7)
}
asp_power <- function(n_rows) {
  switch(as.character(n_rows), "1" = 0.65, "2" = 0.375)
}



# Kernel Selection --------------------------------------------------------

# Line and point sizes for line plots
line_size_kersel <- 0.6
point_size_kersel <- 2.5

# color scheme for kernel selection plot
colors_kersel <- function(n_colors) {
  colors_inferno(n_colors) # default: begin = 0, end = 0.85, direction = -1
}

theme_kersel <- function(
  base_theme = theme_minimal,
  base_size = 11,
  show_legend_title = TRUE,
  legend_position = "bottom",
  legend_margins = margin(1, 1, 1, 1),
  legend_title_size = rel(1),
  legend_text_size = rel(0.8),
  legend_symbol_size = 12,
  legend_symbol_spacing = 10,
  fontsize_x = rel(0.9),
  fontsize_y = rel(0.9),
  fontsize_title = rel(1.4),
  fontsize_subtitle = rel(1),
  fontsize_x_axis = rel(0.9),
  fontsize_y_axis = rel(1),
  plot_margin = margin(b = 15),
  aspect = 0.9,
  title_position = "panel") {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect,
    title_position)
}

title_kersel <- function(
  joint_title,
  joint_subtitle,
  joint_title_size = 18,
  joint_subtitle_size = 11,
  joint_title_height = 0.05,
  title_overlay_margins = margin(t = 7, l = 7),
  joint_subtitle_height = 0.12,
  # subtitle_overlay_margins = margin(t = -10, b = 10, l = 7),
  subtitle_overlay_margins = margin(b = 7, l = 7),
  joint_legend_width = 0.25,
  joint_legend_height = 0.05,
  joint_legend_margins = margin(t = 10, b = 10)) {
  makeTitleSettings(
    joint_title, joint_subtitle,
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins,
    joint_legend_width, joint_legend_height, joint_legend_margins)
}
makeKerSelTitle <- function(y_ind) {
  switch(
    as.character(y_ind),
    "1" = expression(paste(
      "Kernel Selection Rates for Y"[1], " Across Data Replicates")),
    "2" = expression(paste(
      "Kernel Selection Rates for Y"[2], " Across Data Replicates")),
    "3" = expression(paste(
      "Kernel Selection Rates for Y"[3], " Across Data Replicates")),
    "4" = expression(paste(
      "Kernel Selection Rates for Y"[4], " Across Data Replicates"))
  )
}
makeKerSelSubtitle <- function(x_type,
                               rho = NULL # only used for cts x_type
) {

  # if (error_distribution == 'cauchy') { err_dist <- 'Cauchy ' }
  # if (error_distribution == 'normal') { err_dist <- 'normal ' }
  # signal_label <- paste0(100 * signal_strength, '% signal strength')
  if (x_type == 'cts') {
    if (rho == 0) {
      # error_corr_label <- 'errors with uncorrelated components'
      error_corr_label <- '\nUncorrelated error components'
    } else {
      error_corr_label <-
        # paste0('errors with correlated components (\u00B1', '0.5 pairwise)')
        paste0('\nCorrelated error components (\u00B1', '0.5 pairwise)')
    }
    out <- paste0(
      'Continuous features',
      # signal_label,
      # '\nMultivariate ',
      # err_dist,
      error_corr_label,
      '\nKernel selection rates across ', num_replicates,
      ' simulated data replicates')
  }
  if (x_type == 'snp') {
    out <- paste0('Simulated SNP set (p = ', p, ')')
    if (signal_correlation == 'high') {
      out <- paste0(out, ' with strongly-correlated signal variables')
    }
    if (signal_correlation == 'low') {
      out <- paste0(out, ' with weakly-correlated signal variables')
    }
    out <- paste0(out,
                  # signal_label,
                  # '\nMultivariate ', err_dist, 'errors',
                  '\nMean kernel selection rate across ', num_replicates,
                  ' simulated data replicates')
  }
  return(out)
}

# Joint plot size
height_kersel <- 7
asp_kersel <- function(n_cols) {
  switch(as.character(n_cols), "2" = 0.45, "3" = 0.375)
}


# Feature Selection -------------------------------------------------------


# Line and point sizes for line plots
line_size_phimr <- 0.6
point_size_phimr <- 2.5

# color scheme for kernel selection plot
colors_phimr <- function(n_colors) {
  colors_inferno(n_colors)
}

# point shapes for line plots (used for main plot only)
shapes_phimr <- c(
  # 15, # solid square
  # 19, # solid circle
  # 17, # solid triangle
  # 18, # solid diamond
  # 0, # square outline
  # 5, # diamond outline
  # 2, # triangle outline
  6, # lower triangle outline
  # 1, # circle outline
  4, # X
  3,  # cross
  20, # solid small circle
  7, 8, 9, 10, 11, 12, 13, 14, 25, 21, 22, 23, 24
)
theme_phimr <- function(
  base_theme = theme_minimal,
  base_size = 11,
  show_legend_title = TRUE,
  legend_position = "right",
  legend_margins = margin(1, 1, 1, 1),
  legend_title_size = rel(1),
  legend_text_size = rel(0.8),
  legend_symbol_size = 12,
  legend_symbol_spacing = 10,
  fontsize_x = rel(0.9),
  fontsize_y = rel(0.9),
  fontsize_title = rel(1.4),
  fontsize_subtitle = rel(1),
  fontsize_x_axis = rel(0.9),
  fontsize_y_axis = rel(1.0),
  plot_margin = margin(b = 15),
  aspect = 0.9,
  title_position = "panel") {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect,
    title_position)
}

title_phimr <- function(
  joint_title = "PhiMr Retention Rates for Signal/Noise Variables",
  joint_subtitle = makePhimrSubtitle(),
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.04,
  title_overlay_margins = margin(t = 7, l = 7),
  joint_subtitle_height = 0.09,
  # subtitle_overlay_margins = margin(t = -10, b = 10, l = 7),
  subtitle_overlay_margins = margin(b = 7, l = 7),
  joint_legend_width = 0.35,
  joint_legend_height = 0.05,
  joint_legend_margins = margin(l = 20, r = -20)) {
  makeTitleSettings(
    joint_title, joint_subtitle,
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins,
    joint_legend_width, joint_legend_height, joint_legend_margins)
}

makePhimrSubtitle <- function(type = "retention") {
  if (x_type == 'cts') feature_type <- 'Continuous features'
  if (x_type == 'snp') {
    feature_type <- 'Simulated SNP set (p = 567)'
    if (signal_correlation == 'high') feature_type <-
        paste0(feature_type, ' with strongly-correlated signal variables')
    if (signal_correlation == 'low') feature_type <-
        paste0(feature_type, ' with weakly-correlated signal variables')
  }
  # if (error_distribution == 'cauchy') { err_dist <- 'Cauchy ' }
  # if (error_distribution == 'normal') { err_dist <- 'normal ' }
  # signal_label <- paste0(100 * signal_strength, '% signal strength')
  out <- paste0(feature_type
                # signal_label,
                # '\nMultivariate ', err_dist, 'errors'
  )
  if (type == "retention") {
    out <- paste0(
      out,
      '\nMean share of signal/noise variables retained by PhiMr across ',
      num_replicates, ' simulated data replicates')
  }
  if (type == "density") {
    out <- paste0(
      out,
      '\nMean value, across ', num_replicates, ' data replicates, ',
      'of signal density after PhiMr relative to before',
      '\nSignal density measured as number of signal variables relative to feature dimension')
  }
  return(out)
}

# Joint plot size
height_phimr <- 7
asp_phimr <- 0.5


# Adaptive Within ---------------------------------------------------------
# Variability of AMKAT P-value and PhiMr filter on a fixed data set

### Stat Distribution ###
height_statDistr = 4
asp_statDistr = 0.85
theme_statDistr <- function(base_theme = theme_minimal,
                            base_size = 11,
                            legend_position = "none",
                            fontsize_x = rel(1),
                            fontsize_y = rel(0.9),
                            fontsize_title = rel(1.2),
                            fontsize_subtitle = rel(0.9),
                            fontsize_x_axis = rel(0.9),
                            fontsize_y_axis = rel(1),
                            plot_margin = margin(l = 5, r = 5),
                            aspect = 0.8,
                            title_position = "panel") {
  makeTheme(base_theme = base_theme, base_size = base_size,
            legend_position = legend_position,
            fontsize_x = fontsize_x, fontsize_y = fontsize_y,
            fontsize_title = fontsize_title,
            fontsize_subtitle = fontsize_subtitle,
            fontsize_x_axis = fontsize_x_axis,
            fontsize_y_axis = fontsize_y_axis,
            plot_margin = plot_margin, aspect = aspect,
            title_position = title_position)
}
title_statDistr <- function(
  joint_title = 'Distribution of AMKAT Statistics on a Fixed Data Set',
  joint_subtitle = makeScenarioDesc(),
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.09,
  title_overlay_margins = margin(t = 7, b = 7, l = 7),
  joint_subtitle_height = 0.09,
  subtitle_overlay_margins = margin(t = -20, b = -20, l = 7)
) {
  makeTitleSettings(joint_title, joint_subtitle, joint_title_size,
                    joint_subtitle_size, joint_title_height,
                    title_overlay_margins, joint_subtitle_height,
                    subtitle_overlay_margins)
}

### Pvalue Distribution vs Q ###
# Plot height and aspect ratio
height_violinQ <- 6.5
asp_violinQ <- 1.55
# B values used for row groups in Violin plot of Pval distribution vs Q
B_values_violinQ <- c(500, 1000, 3000, 5000)
color_code_violinQ <- "H" # (H = turbo)
theme_violinQ <- function(base_size = 13,
                          fontsize_x = rel(1),
                          fontsize_y = rel(1),
                          fontsize_title = rel(1),
                          fontsize_subtitle = rel(1),
                          fontsize_x_axis = rel(1.1),
                          fontsize_y_axis = rel(1),
                          plot_margin = margin(l = 5, r = 5),
                          aspect = 0.08,
                          x_axis_margin = 5) {
  theme_violin(base_size, fontsize_x, fontsize_y, fontsize_title,
               fontsize_subtitle, fontsize_x_axis, fontsize_y_axis,
               plot_margin, aspect, x_axis_margin)
}
title_violinQ <- function(
  num_replicates,
  joint_title =
    'Distribution of AMKAT P-value on a Fixed Data Set',
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.05,
  title_overlay_margins = margin(t = 7, b = 0, l = 7),
  joint_subtitle_height = 0.14,
  subtitle_overlay_margins = margin(t = -25, b = -10, l = 7)
) {
  makeTitleSettings(
    joint_title, joint_subtitle =
      paste0(makeScenarioDesc(),
             '\nEach sample contains ', num_replicates, ' P-values'),
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins)
}

### Pvalue Distribution vs B ###
# Plot height and aspect ratio
height_violinB <- 7.5
asp_violinB <- 0.95
# Q values used for row groups in Violin plot of Pval distribution vs B
color_code_violinB <- "C" # (D = viridis)
Q_values_violinB <- c(1, 32, 128, 512)
theme_violinB <- function(base_size = 11,
                          fontsize_x = rel(1),
                          fontsize_y = rel(1),
                          fontsize_title = rel(1),
                          fontsize_subtitle = rel(1),
                          fontsize_x_axis = rel(1.1),
                          fontsize_y_axis = rel(1),
                          plot_margin = margin(l = 5, r = 5),
                          aspect = 0.1,
                          x_axis_margin = 5) {
  theme_violin(base_size, fontsize_x, fontsize_y, fontsize_title,
               fontsize_subtitle, fontsize_x_axis, fontsize_y_axis,
               plot_margin, aspect, x_axis_margin)
}
title_violinB <- function(
  num_replicates,
  joint_title =
    'Distribution of AMKAT P-value on a Fixed Data Set',
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.04,
  title_overlay_margins = margin(t = 7, b = 0, l = 7),
  joint_subtitle_height = 0.12,
  subtitle_overlay_margins = margin(t = -25, b = -10, l = 7)) {

  makeTitleSettings(
    joint_title, joint_subtitle =
      paste0(makeScenarioDesc(),
             '\nEach sample contains ', num_replicates, ' P-values'),
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins)
}

### Pvalue Mean/SD ###
# Q values used for line groups in line plot of Pval SD/mean vs B
Q_values_line <- c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)

# colors for line groups (grouped by Q) in plot of Pval SD/mean vs B
Q_colors_line <- viridis::turbo(n = length(Q_values_line) + 1,
                                begin = 0, end = 1, direction = 1)

# shapes for line groups (grouped by Q) in plot of Pval SD/mean vs B
Q_shapes_line <- c(15, # solid square             NF
                   17, # solid triangle           PF-1
                   20, # solid small circle,      PF-2
                   18, # solid diamond            PF-4
                   0, # square outline            PF-8
                   5, # diamond outline           PF-16
                   2, # triangle outline          PF-32
                   6, # lower triangle outline    PF-64
                   1, # circle outline            PF-128
                   4, # X                         PF-256
                   3  # cross                     PF-512
)
# B values used for line groups in line plot of Pval SD/mean vs Q
B_values_line <- c(500, 1000, 2000, 3000, 4000, 5000)

# colors for line groups (grouped by B) in plot of Pval SD/mean vs Q
B_colors_line <- viridis::plasma(length(B_values_line),
                                 begin = 0, end = 0.85, direction = 1)

# shapes for line groups (grouped by B) in plot of Pval SD/mean vs Q
B_shapes_line <- c(15, # solid square,
                   20, # solid small circle,
                   5, # diamond outline
                   6, # lower triangle outline
                   4, # X
                   3, # cross
                   0, # square outline
                   17, # solid triangle
                   18 # solid diamond
)
line_size_pv <- 0.6
point_size_pv <- 2.5
theme_pvline <- function(base_theme = theme_minimal,
                         base_size = 11,
                         show_legend_title = TRUE,
                         legend_position = "right",
                         legend_margins = margin(1, 1, 1, 1),
                         legend_title_size = rel(1),
                         legend_text_size = rel(0.8),
                         legend_symbol_size = 12,
                         legend_symbol_spacing = 10,
                         fontsize_x = rel(0.9),
                         fontsize_y = rel(0.9),
                         fontsize_title = rel(1.1),
                         fontsize_subtitle = rel(0.9),
                         fontsize_x_axis = rel(1),
                         fontsize_y_axis = rel(1),
                         plot_margin = margin(),
                         aspect = 1,
                         title_position = "plot") {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect,
    title_position)
}
title_pvline <- function(
  joint_title =
    "Standard Deviation of AMKAT P-value on a Fixed Data Set",
  joint_subtitle = makeScenarioDesc(),
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.09,
  title_overlay_margins = margin(t = 7),
  joint_subtitle_height = 0.15,
  subtitle_overlay_margins = margin(t = -15)) {
  makeTitleSettings(
    joint_title, joint_subtitle,
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins)
}
# Plot height and aspect ratio
height_pvline <- 5
asp_pvline <- 0.95


theme_phimr_histo <- function(
  base_theme = theme_minimal,
  base_size = 11,
  show_legend_title = FALSE,
  legend_position = "none",
  legend_margins = margin(1, 1, 1, 1),
  legend_title_size = rel(1),
  legend_text_size = rel(0.8),
  legend_symbol_size = 12,
  legend_symbol_spacing = 10,
  fontsize_x = rel(1),
  fontsize_y = rel(1.1),
  fontsize_title = rel(1.3),
  fontsize_subtitle = rel(1),
  fontsize_x_axis = rel(1.2),
  fontsize_y_axis = rel(1.3),
  plot_margin = margin(b = 7),
  aspect = 0.8,
  title_position = "panel") {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect,
    title_position)
}


title_phimr_histo <- function(
  joint_title =
    "Distribution of Variable Retention Rates by PhiMr on a Fixed Data Set",
  joint_subtitle =
    paste0(
      makeScenarioDesc(),
      "\nEach observation corresponds to a distinct variable in the original feature set",
      "\nRetention rate is the proportion of times the variable was selected across ",
      num_test_statistics * num_replicates, " applications of PhiMr"),
  joint_title_size = 16,
  joint_subtitle_size = 13,
  joint_title_height = 0.1,
  title_overlay_margins = margin(t = 7, l = 7),
  joint_subtitle_height = 0.275,
  subtitle_overlay_margins = margin(t = -10, l = 7)) {
  makeTitleSettings(
    joint_title,
    joint_subtitle,
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins)
}
height_phimr_within <- 5 #subplot height
asp_phimr_within <- 0.9 #subplot aspect ratio


# Runtime Benchmarking ----------------------------------------------------
colors_bench <- function(n_colors) {
  colors_inferno(n_colors) # default: begin = 0.15, end = 0.85, direction = 1
}

### KERNEL MATRIX ###
height_bench_ker <- 4.5 #subplot height
asp_bench_ker <- 0.53 #subplot aspect ratio
timefactor_ker <- 1e+6 # milliseconds
timelabel_ker <- "Median Time (ms)"
line_size_bench_ker <- 0.6
point_size_bench_ker <- 2
theme_bench_ker <- function(base_theme = theme_minimal,
                            base_size = 11,
                            show_legend_title = TRUE,
                            legend_position = "bottom",
                            legend_margins = margin(t = -20),
                            legend_title_size = rel(1),
                            legend_text_size = rel(0.9),
                            legend_symbol_size = 12,
                            legend_symbol_spacing = 0,
                            fontsize_x = rel(9/11),
                            fontsize_y = rel(9/11),
                            fontsize_title = rel(1),
                            fontsize_subtitle = rel(10/11),
                            fontsize_x_axis = rel(0.9),
                            fontsize_y_axis = rel(0.9),
                            plot_margin = margin(t = -25, b = -30,
                                                 l = 7, r = 7),
                            aspect = 1.5) {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect)
}

title_bench_ker <- function(
  num_executions,
  joint_title = "Time Taken to Generate Kernel Matrix",
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.12,
  title_overlay_margins = margin(t = 0, l = 7),
  joint_subtitle_height = 0.04,
  subtitle_overlay_margins = margin(t = -7, l = 7),
  joint_legend_width = 0.1,
  joint_legend_height = 0.005,
  joint_legend_margins = margin(t = -25, b = -15)) {
  makeTitleSettings(
    joint_title,
    joint_subtitle = paste0(
      "Median time over ", num_executions,
      " executions using the generateKernelMatrix() function from the AMKAT R package"),
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins,
    joint_legend_width, joint_legend_height, joint_legend_margins)
}

### PHIMR FILTER ###
height_bench_phimr <- 5 #subplot height
asp_bench_phimr <- 1.02 #subplot aspect ratio
timefactor_phimr <- 1e+6 # milliseconds
timelabel_phimr <- "Median Time (ms)"
line_size_bench_phimr <- 0.6
point_size_bench_phimr <- 2.5
theme_bench_phimr <- function(base_theme = theme_minimal,
                              base_size = 14,
                              show_legend_title = TRUE,
                              legend_position = "right",
                              legend_margins = margin(1, 1, 1, 1),
                              legend_title_size = rel(0.85),
                              legend_text_size = rel(0.75),
                              legend_symbol_size = 12,
                              legend_symbol_spacing = 0,
                              fontsize_x = rel(0.8),
                              fontsize_y = rel(0.8),
                              fontsize_title = rel(1.2),
                              fontsize_subtitle = rel(0.8),
                              fontsize_x_axis = rel(0.9),
                              fontsize_y_axis = rel(0.9),
                              plot_margin = margin(l = 7, r = 7),
                              aspect = 1) {

  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect)
}

### AMKAT FULL TEST ###
height_bench_amkat <- 5 #subplot height
asp_bench_amkat <- 0.775 #subplot aspect ratio
timefactor_amkat <- 6e+10 # minutes
timelabel_amkat <- "Execution Time (minutes)"
line_size_bench_amkat <- 0.6
point_size_bench_amkat <- 2
theme_bench_amkat <- function(base_theme = theme_minimal,
                              base_size = 11,
                              show_legend_title = TRUE,
                              legend_position = "right",
                              legend_margins = margin(1, 1, 1, 1),
                              legend_title_size = rel(1),
                              legend_text_size = rel(0.9),
                              legend_symbol_size = 12,
                              legend_symbol_spacing = 10,
                              fontsize_x = rel(0.9),
                              fontsize_y = rel(0.9),
                              fontsize_title = rel(1.2),
                              fontsize_subtitle = rel(0.9),
                              fontsize_x_axis = rel(0.9),
                              fontsize_y_axis = rel(0.9),
                              plot_margin = margin(t = 5),
                              aspect = 1) {
  makeTheme(
    base_theme, base_size, show_legend_title, legend_position, legend_margins,
    legend_title_size, legend_text_size, legend_symbol_size,
    legend_symbol_spacing, fontsize_x, fontsize_y, fontsize_title,
    fontsize_subtitle, fontsize_x_axis, fontsize_y_axis, plot_margin, aspect)
}
title_bench_amkat <- function(
  num_permutations,
  joint_title = "AMKAT Execution Time",
  joint_title_size = 16,
  joint_subtitle_size = 11,
  joint_title_height = 0.1,
  title_overlay_margins = margin(t = 0, l = 7),
  joint_subtitle_height = 0.1,
  subtitle_overlay_margins = margin(t = 2, l = 7),
  joint_legend_width = 0.15,
  joint_legend_height = 0.005,
  joint_legend_margins = margin(l = 5, r = 5)) {
  makeTitleSettings(
    joint_title, joint_subtitle =
      paste0(
        "4 response variables; 2 covariates; continuous features",
        "\nCandidate kernels include linear, quadratic, Gaussian and IBS",
        "\n", num_permutations, " permutations used to estimate each P-value"),
    joint_title_size, joint_subtitle_size, joint_title_height,
    title_overlay_margins, joint_subtitle_height, subtitle_overlay_margins,
    joint_legend_width, joint_legend_height, joint_legend_margins)
}
