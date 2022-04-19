# Kernel matrix
if (!file.exists(data_filepath("kermat")) | overwrite_existing_data) {
  ker_reps <- num_replicates * 400 # executions per worker
  set.seed(seed_value)
  times_kermat_long <-
    foreach::foreach(
      chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
      .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
        foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
          c(
            microbenchmark(generateKernelMatrix(x, "lin"),
                           times = ker_reps)$time,
            microbenchmark(generateKernelMatrix(x, "quad"),
                           times = ker_reps)$time,
            microbenchmark(generateKernelMatrix(x, "gau"),
                           times = ker_reps)$time,
            microbenchmark(generateKernelMatrix(x, "exp"),
                           times = ker_reps)$time,
            microbenchmark(generateKernelMatrix(x, "IBS"),
                           times = ker_reps)$time
          )
        }
      }
  times_kermat <- data.frame(
    "lin" = as.vector(times_kermat_long[1:ker_reps, ]),
    "quad" = as.vector(times_kermat_long[(ker_reps + 1):(2 * ker_reps), ]),
    "gau" = as.vector(times_kermat_long[(2 * ker_reps + 1):(3 * ker_reps), ]),
    "exp" = as.vector(times_kermat_long[(3 * ker_reps + 1):(4 * ker_reps), ]),
    "IBS" = as.vector(times_kermat_long[(4 * ker_reps + 1):(5 * ker_reps), ])
  )

  save(times_kermat, file = data_filepath("kermat"))
}



# PhiMr filter
if (!file.exists(data_filepath("phimr")) | overwrite_existing_data) {
  phimr_reps <- num_replicates * 400
  set.seed(seed_value)
  times_phimr <- as.vector(
    foreach::foreach(
      chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
      .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
        foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
          microbenchmark("phimr" = phimr(y, x), times = phimr_reps)$time
        }
      }
  )

  save(times_phimr, file = data_filepath("phimr"))
}


# AMKAT full test
if (!file.exists(data_filepath("amkat")) | overwrite_existing_data) {
  set.seed(seed_value)
  time_amkat_nofilter <- microbenchmark(
    amkat(y, x, covariates, filter_x = FALSE,
          candidate_kernels = bench_kernels,
          num_permutations = num_permutations,
          num_test_statistics = 1),
    times = num_replicates)$time
  set.seed(seed_value)
  time_amkat_phimr <- microbenchmark(
    amkat(y, x, covariates, filter_x = TRUE,
          candidate_kernels = bench_kernels,
          num_permutations = num_permutations,
          num_test_statistics = 50),
    times = num_replicates)$time
  times_amkat <- data.frame(
    "nofilter" = time_amkat_nofilter,
    "phimr50" = time_amkat_phimr
  )

  save(times_amkat, file = data_filepath("amkat"))
}