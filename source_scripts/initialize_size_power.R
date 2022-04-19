
# Settings for Simulating Data --------------------------------------------

# Default parameter values
if (size_or_power == 'size') values_for_signal_strength <- 1
if (size_or_power == 'size') values_for_signal_density <- 'sparse'
if (x_type == 'snp') values_for_num_x_variables <- 567

# Max number of test statistics to use for AMKAT with PhiMr filter
num_test_statistics <- 128L

# Additional methods for power scenarios
method_names <- method_names_short <- character()
if (size_or_power == 'power') {
  method_names <-
    c(method_names, c('AMKAT_nofilter_pseudo', 'AMKAT_nofilter_floor'))
  method_names_short <- c(method_names_short, 'AMKAT_nofilter')
}

# method names: all methods
method_names <-
  c(method_names,
    c('AMKAT_pseudo', 'AMKAT_floor',
      # paste0(method_label_mpv10, '_pseudo'),
      # paste0(method_label_mpv10, '_floor'),
      # paste0(method_label_mpv20, '_pseudo'),
      # paste0(method_label_mpv20, '_floor'),
      # paste0(method_label_mpv50, '_pseudo'),
      # paste0(method_label_mpv50, '_floor'),
      'AMKAT_avgstat_2_pseudo', 'AMKAT_avgstat_2_floor',
      'AMKAT_avgstat_4_pseudo', 'AMKAT_avgstat_4_floor',
      'AMKAT_avgstat_8_pseudo', 'AMKAT_avgstat_8_floor',
      'AMKAT_avgstat_16_pseudo', 'AMKAT_avgstat_16_floor',
      'AMKAT_avgstat_32_pseudo', 'AMKAT_avgstat_32_floor',
      'AMKAT_avgstat_64_pseudo', 'AMKAT_avgstat_64_floor',
      'AMKAT_avgstat_128_pseudo', 'AMKAT_avgstat_128_floor',
      'OMGA_original_Y', 'OMGA_residual_Y', 'DKAT', 'DK_L'))

# method names: select subset of methods
method_names_short <-
  c(method_names_short,
    c('AMKAT',
      # method_label_mpv10, method_label_mpv20, method_label_mpv50,
      'AMKAT_avgstat_2', 'AMKAT_avgstat_4', 'AMKAT_avgstat_8',
      'AMKAT_avgstat_16', 'AMKAT_avgstat_32', 'AMKAT_avgstat_64',
      'AMKAT_avgstat_128',
      'OMGA_original_Y', 'OMGA_residual_Y', 'DKAT', 'DK_L'))



# Deprecated:
# # Number of AMKAT tests to perform at a given # of permutations per test
# # (relevant for AMKAT mean P-value method)
# num_tests_at_10_perms_each <- num_permutations / 10
# num_tests_at_20_perms_each <- num_permutations / 20
# num_tests_at_50_perms_each <- num_permutations / 50
# # Number of AMKAT test statistics to compute per data set
# # (relevant for AMKAT mean observed statistic method)
# num_test_statistics <- max(num_tests_at_10_perms_each, 128)
# # AMKAT mean P-value method labels
# method_label_mpv10 <- paste0('AMKAT_MPv_10x', num_tests_at_10_perms_each)
# method_label_mpv20 <- paste0('AMKAT_MPv_20x', num_tests_at_20_perms_each)
# method_label_mpv50 <- paste0('AMKAT_MPv_50x', num_tests_at_50_perms_each)




# Settings for Preparing Plot Data ----------------------------------------

# methods for main power plot
chosen_methods_main <- c('OMGA_residual_Y', 'DKAT', 'DK_L',
                         'AMKAT_nofilter_pseudo',
                         'AMKAT_avgstat_64_pseudo',
                         'AMKAT_avgstat_128_pseudo')
# method labels for main plot legend
method_labels_main <-
  c('\nOMGA\n', '\nDKAT\n', 'DKAT with Linear\nPhenotype Kernel',
    '\nAMKAT (no filter)\n',
    'AMKAT (PhiMr filter)\nwith 64 test stats',
    '\nAMKAT (PhiMr filter)\nwith 128 test stats\n')


# Methods for AMKAT methods plot
chosen_methods_amkat <-
  c('AMKAT_nofilter_pseudo', 'AMKAT_pseudo',
    'AMKAT_avgstat_2_pseudo', 'AMKAT_avgstat_4_pseudo',
    'AMKAT_avgstat_8_pseudo', 'AMKAT_avgstat_16_pseudo',
    'AMKAT_avgstat_32_pseudo', 'AMKAT_avgstat_64_pseudo',
    'AMKAT_avgstat_128_pseudo')

# method labels for AMKAT methods plot legend
method_labels_amkat <- c('0 (no filter)', paste0(2 ^ (0:7)))