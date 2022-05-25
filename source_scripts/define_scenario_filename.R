
# File name stem for power scenarios
if (size_or_power == 'power') {
  if (x_type == 'cts') {
    scenario_filename_stem <-
      paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
             '_m', num_replicates, '_', x_type, '_', error_distribution,
             '_s', signal_strength * 100, '_', signal_density,
             '_c', error_correlation_strength * 100, '_n', n, '_p', p)
  }
  if (x_type == 'snp') {
    scenario_filename_stem <-
      paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
             '_m', num_replicates, '_', x_type, '_', error_distribution,
             '_s', signal_strength * 100, '_sc-', signal_correlation,
             '_', signal_density,
             '_c', error_correlation_strength * 100, '_n', n, '_p', p)
  }
}

# File name stem for size scenarios
if (size_or_power == 'size') scenario_filename_stem <-
    paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations,
           '_m', num_replicates, '_', x_type, '_', error_distribution,
           '_c', error_correlation_strength * 100, '_n', n, '_p', p)
