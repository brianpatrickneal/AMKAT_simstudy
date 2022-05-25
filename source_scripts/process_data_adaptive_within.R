
# Load and Process Data ---------------------------------------------------

# Process statistics to compute P-values
for (error_distribution in values_for_error_distribution) {
  for (signal_strength in values_for_signal_strength) {
    for (signal_correlation in values_for_snp_signal_corr) {
      for (signal_density in values_for_signal_density) {
        for (error_correlation_strength in values_for_error_corr_strength) {
          for (n in values_for_sample_size) {
            for (p in values_for_num_x_variables) {

              source(file.path(dir_src, 'define_scenario_filename.R'))

              data_filepath_stem <- file.path(dir_data_adaptive_within,
                                              scenario_filename_stem)
              input_filepath <- paste0(data_filepath_stem, '_raw.Rdata')
              output_filepath <- paste0(data_filepath_stem, '_pvalues.Rdata')

              # Check first whether file already exists
              if (!file.exists(output_filepath) | reprocess_existing_data ) {

                load(file = input_filepath)

                source(file.path(dir_src,
                                 'process_scenario_data_adaptive_within.R'))

                save('pvalues', file = output_filepath)

              }
            }
          }
        }
      }
    }
  }
}
