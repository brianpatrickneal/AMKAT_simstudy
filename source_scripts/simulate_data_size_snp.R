# ------------------------------------------------------------------------------
data <-
  foreach::foreach(
    chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
    .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
      foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
        out <- double()
        covariates <- simulateDataCovariates()
        x <- simulateDataX()
        y <- simulateDataY(x, covariates)
        covdim <- ncol(covariates)
        w_aug <- cbind(rep(1, times = n), covariates)
        hat_matrix <-
          w_aug %*% tcrossprod(solve(crossprod(w_aug, w_aug)), w_aug)
        null_residuals <- (diag(n) - hat_matrix) %*% y
        null_standard_errors <-
          diag(crossprod(y, null_residuals)) / (n - covdim - 1)

        # Statistics for AMKAT
        observed_statistics <- AMKAT:::.generateTestStatMultiple(
          null_residuals, null_standard_errors, x, candidate_kernels,
          num_test_statistics)
        permutation_statistics <- AMKAT:::.generatePermStats(
          null_residuals, null_standard_errors, x, candidate_kernels,
          num_permutations)

        # AMKAT
        p_value <- mean(permutation_statistics >= observed_statistics[[1]])
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor

        # # AMKAT Mean P-value
        # #  10 permutations per test
        # p_value <-
        #   mean(apply(rbind(matrix(permutation_statistics,
        #                           nrow = 10, ncol = num_tests_at_10_perms_each),
        #                    observed_statistics[1:num_tests_at_10_perms_each]),
        #              MARGIN = 2,
        #              FUN = function(x){mean(x[1:10] >= x[[10 + 1]])}))
        # out <- c(out, p_value + pseudocount)
        # out <- c(out, max(p_value, pseudocount)) # floor
        #
        # #  20 permutations per test
        # p_value <-
        #   mean(apply(rbind(matrix(permutation_statistics,
        #                           nrow = 20, ncol = num_tests_at_20_perms_each),
        #                    observed_statistics[1:num_tests_at_20_perms_each]),
        #              MARGIN = 2,
        #              FUN = function(x){mean(x[1:20] >= x[[20 + 1]])}))
        # out <- c(out, p_value + pseudocount)
        # out <- c(out, max(p_value, pseudocount)) # floor
        #
        # #  50 permutations per test
        # p_value <-
        #   mean(apply(rbind(matrix(permutation_statistics,
        #                           nrow = 50, ncol = num_tests_at_50_perms_each),
        #                    observed_statistics[1:num_tests_at_50_perms_each]),
        #              MARGIN = 2,
        #              FUN = function(x){mean(x[1:50] >= x[[50 + 1]])}))
        # out <- c(out, p_value + pseudocount)
        # out <- c(out, max(p_value, pseudocount)) # floor

        # AMKAT Mean Observed Statistic
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:2]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:4]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:8]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:16]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:32]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:64]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor
        p_value <-
          mean(permutation_statistics >= mean(observed_statistics[1:128]))
        out <- c(out, p_value + pseudocount)
        out <- c(out, max(p_value, pseudocount)) # floor

        # Kernel matrices for OMGA and DKAT
        ker_mat_gau <- generateKernelMatrix(x, 'gau')
        ker_mat_lin <- generateKernelMatrix(x, 'lin')
        ker_mat_quad <- generateKernelMatrix(x, 'quad')
        ker_mat_exp <- generateKernelMatrix(x, 'exp')
        ker_mat_ibs <- generateKernelMatrix(x, 'IBS')
        ker_mat_gau <- ker_mat_gau / sum(diag(ker_mat_gau))
        ker_mat_lin <- ker_mat_lin / sum(diag(ker_mat_lin))
        ker_mat_quad <- ker_mat_quad / sum(diag(ker_mat_quad))
        ker_mat_exp <- ker_mat_exp / sum(diag(ker_mat_exp))
        ker_mat_ibs <- ker_mat_ibs / sum(diag(ker_mat_ibs))
        kernel_matrix_x <-
          ker_mat_gau + ker_mat_lin + ker_mat_quad + ker_mat_exp + ker_mat_ibs

        # OMGA
        ker_mat_gau <- ker_mat_gau * n
        ker_mat_lin <- ker_mat_lin * n
        ker_mat_quad <- ker_mat_quad * n
        ker_mat_exp <- ker_mat_exp * n
        ker_mat_ibs <- ker_mat_ibs * n
        pvalues_combined_cauchy <- rep(0, num_y_variables)
        p_values <- rep(0, length(candidate_kernels))
        p_values[[1]] <- kernelUTestAsymptotic(
          null_residuals[, 1], null_standard_errors[[1]], ker_mat_gau)
        p_values[[2]] <- kernelUTestAsymptotic(
          null_residuals[, 1], null_standard_errors[[1]], ker_mat_lin)
        p_values[[3]] <- kernelUTestAsymptotic(
          null_residuals[, 1], null_standard_errors[[1]], ker_mat_quad)
        p_values[[4]] <- kernelUTestAsymptotic(
          null_residuals[, 1], null_standard_errors[[1]], ker_mat_exp)
        p_values[[5]] <- kernelUTestAsymptotic(
          null_residuals[, 1], null_standard_errors[[1]], ker_mat_ibs)
        omnibus_statistic <- sum(tan(pi * (0.5 - p_values)) / n)
        pvalues_combined_cauchy[[1]] <- 1 - pcauchy(omnibus_statistic)
        p_values[[1]] <- kernelUTestAsymptotic(
          null_residuals[, 2], null_standard_errors[[2]], ker_mat_gau)
        p_values[[2]] <- kernelUTestAsymptotic(
          null_residuals[, 2], null_standard_errors[[2]], ker_mat_lin)
        p_values[[3]] <- kernelUTestAsymptotic(
          null_residuals[, 2], null_standard_errors[[2]], ker_mat_quad)
        p_values[[4]] <- kernelUTestAsymptotic(
          null_residuals[, 2], null_standard_errors[[2]], ker_mat_exp)
        p_values[[5]] <- kernelUTestAsymptotic(
          null_residuals[, 2], null_standard_errors[[2]], ker_mat_ibs)
        omnibus_statistic <- sum(tan(pi * (0.5 - p_values)) / n)
        pvalues_combined_cauchy[[2]] <- 1 - pcauchy(omnibus_statistic)
        p_values[[1]] <- kernelUTestAsymptotic(
          null_residuals[, 3], null_standard_errors[[3]], ker_mat_gau)
        p_values[[2]] <- kernelUTestAsymptotic(
          null_residuals[, 3], null_standard_errors[[3]], ker_mat_lin)
        p_values[[3]] <- kernelUTestAsymptotic(
          null_residuals[, 3], null_standard_errors[[3]], ker_mat_quad)
        p_values[[4]] <- kernelUTestAsymptotic(
          null_residuals[, 3], null_standard_errors[[3]], ker_mat_exp)
        p_values[[5]] <- kernelUTestAsymptotic(
          null_residuals[, 3], null_standard_errors[[3]], ker_mat_ibs)
        omnibus_statistic <- sum(tan(pi * (0.5 - p_values)) / n)
        pvalues_combined_cauchy[[3]] <- 1 - pcauchy(omnibus_statistic)
        p_values[[1]] <- kernelUTestAsymptotic(
          null_residuals[,4], null_standard_errors[[4]], ker_mat_gau)
        p_values[[2]] <- kernelUTestAsymptotic(
          null_residuals[,4], null_standard_errors[[4]], ker_mat_lin)
        p_values[[3]] <- kernelUTestAsymptotic(
          null_residuals[,4], null_standard_errors[[4]], ker_mat_quad)
        p_values[[4]] <- kernelUTestAsymptotic(
          null_residuals[,4], null_standard_errors[[4]], ker_mat_exp)
        p_values[[5]] <- kernelUTestAsymptotic(
          null_residuals[, 4], null_standard_errors[[4]], ker_mat_ibs)
        omnibus_statistic <- sum(tan(pi * (0.5 - p_values)) / n)
        pvalues_combined_cauchy[[4]] <- 1 - pcauchy(omnibus_statistic)
        out <-
          c(out, combinePvaluesFisher(pvalues_combined_cauchy, y)) # raw y
        out <-
          c(out, combinePvaluesFisher(pvalues_combined_cauchy, null_residuals))

        #DKAT
        kernel_matrix_y <-
          null_residuals %*% solve(var(null_residuals)) %*% t(null_residuals)
        out <- c(out, DKAT(kernel_matrix_x, kernel_matrix_y))

        # DKAT with linear kernel for y
        kernel_matrix_y_linear <- tcrossprod(null_residuals, null_residuals)
        out <- c(out, DKAT(kernel_matrix_x, kernel_matrix_y_linear))

        out
      }
    }
rownames(data) <- method_names
null_rejection_rates <- apply(data, 1, function(x){mean(x < alpha)})
rejection_floor <- null_rejection_rates[
  c(seq(from = 2, to = length(null_rejection_rates) - 4, by = 2),
    (length(null_rejection_rates) - 3):length(null_rejection_rates))]
rejection_pseudo <- null_rejection_rates[
  c(seq(from = 1, to = length(null_rejection_rates) - 4, by = 2),
    (length(null_rejection_rates) - 3):length(null_rejection_rates))]
comparison_pseudo_floor <- cbind(rejection_floor, rejection_pseudo)
rownames(comparison_pseudo_floor) <- method_names_short