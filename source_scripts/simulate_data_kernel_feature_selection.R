# ------------------------------------------------------------------------------
data <-
  foreach::foreach(
    chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
    .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
      foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
        covariates <- simulateDataCovariates()
        x <- simulateDataX()
        y <- simulateDataY(x, covariates)
        covdim <- ncol(covariates)
        w_aug <- cbind(rep(1, times = n), covariates)
        hat_matrix <- w_aug %*% tcrossprod(solve(crossprod(w_aug, w_aug)), w_aug)
        null_residuals <- (diag(n) - hat_matrix) %*% y
        null_standard_errors <-
          diag(crossprod(y, null_residuals)) / (n - covdim - 1)
        test_results <- AMKAT:::.generateTestStat(
          null_residuals, null_standard_errors, x, candidate_kernels)
        selected_x_columns <- rep(0, times = p)
        selected_x_columns[test_results$selected_x_columns] <- 1

        c(test_results$selected_kernels, selected_x_columns)
      }
    }
selected_kernels <- data[1:num_y_variables, ]
selected_x_columns <- apply(data[(num_y_variables + 1):nrow(data), ],
                            MARGIN = 2, FUN = as.numeric)