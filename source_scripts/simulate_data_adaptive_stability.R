# -- Simulate Data -------------------------------------------------------------
covariates <- simulateDataCovariates()
x <- simulateDataX()
y <- simulateDataY(x, covariates)
covdim <- ncol(covariates)
w_aug <- cbind(rep(1, times = n), covariates)
hat_matrix <- w_aug %*% tcrossprod(solve(crossprod(w_aug, w_aug)), w_aug)
null_residuals <- (diag(n) - hat_matrix) %*% y
null_standard_errors <- diag(crossprod(y, null_residuals)) / (n - covdim - 1)

# --- Apply Comparison Methods -------------------------------------------------
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
  ker_mat_gau + ker_mat_lin + ker_mat_quad + ker_mat_exp
if (x_type == 'snp') kernel_matrix_x <- kernel_matrix_x + ker_mat_ibs

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
if (x_type == 'snp') {
  p_values[[5]] <- kernelUTestAsymptotic(
    null_residuals[, 1], null_standard_errors[[1]], ker_mat_ibs)
}
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
if (x_type == 'snp') {
  p_values[[5]] <- kernelUTestAsymptotic(
    null_residuals[, 2], null_standard_errors[[2]], ker_mat_ibs)
}
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
if (x_type == 'snp') {
  p_values[[5]] <- kernelUTestAsymptotic(
    null_residuals[, 3], null_standard_errors[[3]], ker_mat_ibs)
}
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
if (x_type == 'snp') {
  p_values[[5]] <- kernelUTestAsymptotic(
    null_residuals[, 4], null_standard_errors[[4]], ker_mat_ibs)
}
omnibus_statistic <- sum(tan(pi * (0.5 - p_values)) / n)
pvalues_combined_cauchy[[4]] <- 1 - pcauchy(omnibus_statistic)
pvalue_omga <- combinePvaluesFisher(pvalues_combined_cauchy, null_residuals)

#DKAT
kernel_matrix_y <-
  null_residuals %*% solve(var(null_residuals)) %*% t(null_residuals)
pvalue_dkat <- DKAT(kernel_matrix_x, kernel_matrix_y)

# DKAT with linear kernel for y
kernel_matrix_y_linear <- tcrossprod(null_residuals, null_residuals)
pvalue_dkat_lin <- DKAT(kernel_matrix_x, kernel_matrix_y_linear)

pvals_comparison_methods <- list(
  'OMGA' = pvalue_omga, 'DKAT' = pvalue_dkat, 'DKAT_LinY' = pvalue_dkat_lin)

# -- Test Using Single Data Set ------------------------------------------------
test_stat_no_filter <-
  AMKAT:::.generateTestStatNoFilter(null_residuals, null_standard_errors,
                                    x, candidate_kernels)
selected_kernels_no_filter <- test_stat_no_filter$selected_kernels
test_stat_no_filter <- test_stat_no_filter$test_statistic

test_results <-
  foreach::foreach(
    chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
    .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
      foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
        out <- double()
        test_results <- AMKAT:::.generateTestStatsAllResults(
          null_residuals, null_standard_errors, x, candidate_kernels,
          num_test_statistics = 100)
        # kernel selection rates
        out <- c(out, apply(test_results$selected_kernels, MARGIN = 2,
                            function(x){mean(x == 'lin')}))
        out <- c(out, apply(test_results$selected_kernels, MARGIN = 2,
                            function(x){mean(x == 'quad')}))
        out <- c(out, apply(test_results$selected_kernels, MARGIN = 2,
                            function(x){mean(x == 'gau')}))
        out <- c(out, apply(test_results$selected_kernels, MARGIN = 2,
                            function(x){mean(x == 'exp')}))
        out <- c(out, apply(test_results$selected_kernels, MARGIN = 2,
                            function(x){mean(x == 'IBS')}))
        out <- c(out, test_results$test_statistics)

        # x variable retention rates
        out <- c(out, apply(test_results$selected_x_columns, MARGIN = 2, mean))

        out
      }
    }
kernel_select_rates <-
  data.frame(matrix(apply(test_results[1:20, ], MARGIN = 1, mean),
                    nrow = num_y_variables, ncol = length(candidate_kernels)),
             row.names = y_col_labels)
names(kernel_select_rates) <- c('lin', 'quad', 'gau', 'exp', 'IBS')
kernel_select_rates$no_filter <- selected_kernels_no_filter
observed_statistics <- test_results[21:120, ]
feature_select_rates <-
  apply(test_results[121:nrow(test_results), ], MARGIN = 1, mean)

permutation_stats <-
  foreach::foreach(
    chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
    .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
      foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
        AMKAT:::.generatePermStats(null_residuals, null_standard_errors, x,
                                   candidate_kernels, num_permutations)
      }
    }
permutation_stats_no_filter <-
  foreach::foreach(
    chunk = iterators::iter(iterator, chunksize = chunk_size), .combine = cbind,
    .packages = packages_to_pass_to_foreach) %dopar% { # pass chunks to workers
      foreach(i = chunk, .combine = cbind) %do% { # worker iterates over chunk
        AMKAT:::.generatePermStatsNoFilter(
          null_residuals, null_standard_errors, x, candidate_kernels,
          num_permutations)
      }
    }