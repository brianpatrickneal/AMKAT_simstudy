# Preliminary objects ----------------------------------------------------------

num_y_variables <- 4 
num_permutations <- max(100, round(num_permutations, digits = -2))
signal_strength <- round(signal_strength, digits = 2)
error_correlation_strength <- round(error_correlation_strength, digits = 2) 
alpha <- round(alpha, digits = 2)
num_tests_at_10_perms_each <- num_permutations / 10 
num_tests_at_20_perms_each <- num_permutations / 20
num_tests_at_50_perms_each <- num_permutations / 50
method_label_mpv10 <- paste0('AMKAT_MPv_10x', num_tests_at_10_perms_each)
method_label_mpv20 <- paste0('AMKAT_MPv_20x', num_tests_at_20_perms_each)
method_label_mpv50 <- paste0('AMKAT_MPv_50x', num_tests_at_50_perms_each)
pseudocount <- 1 / num_permutations  
num_test_statistics <- max(num_tests_at_10_perms_each, 128) 

if (size_or_power == 'power') {
  scenario_filename_stem <- 
    paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations, 
           '_m', num_replicates, '_', x_type, '_', error_distribution, 
           '_s', signal_strength * 100, '_', signal_density, 
           '_c', error_correlation_strength * 100, '_n', n, '_p', p)
}
if (size_or_power == 'size') {
  scenario_filename_stem <- 
    paste0(size_or_power, '_a', alpha * 1000, '_b', num_permutations, 
           '_m', num_replicates, '_', x_type, '_', error_distribution, 
           '_c', error_correlation_strength * 100, '_n', n, '_p', p)
}

method_names <- method_names_short <- character()
if (size_or_power == 'power') {
  method_names <- 
    c(method_names, c('AMKAT_nofilter_pseudo', 'AMKAT_nofilter_floor'))
  method_names_short <- c(method_names_short, 'AMKAT_nofilter')
}
method_names <- 
  c(method_names, 
    c('AMKAT_pseudo', 'AMKAT_floor',
      paste0(method_label_mpv10, '_pseudo'), 
      paste0(method_label_mpv10, '_floor'),
      paste0(method_label_mpv20, '_pseudo'), 
      paste0(method_label_mpv20, '_floor'),
      paste0(method_label_mpv50, '_pseudo'), 
      paste0(method_label_mpv50, '_floor'),
      'AMKAT_avgstat_2_pseudo', 'AMKAT_avgstat_2_floor',
      'AMKAT_avgstat_4_pseudo', 'AMKAT_avgstat_4_floor',
      'AMKAT_avgstat_8_pseudo', 'AMKAT_avgstat_8_floor',
      'AMKAT_avgstat_16_pseudo', 'AMKAT_avgstat_16_floor',
      'AMKAT_avgstat_32_pseudo', 'AMKAT_avgstat_32_floor',
      'AMKAT_avgstat_64_pseudo', 'AMKAT_avgstat_64_floor',
      'AMKAT_avgstat_128_pseudo', 'AMKAT_avgstat_128_floor',
      'OMGA_original_Y', 'OMGA_residual_Y', 'DKAT', 'DK_L'))
method_names_short <- 
  c(method_names_short, 
    c('AMKAT', 
      method_label_mpv10, method_label_mpv20, method_label_mpv50,
      'AMKAT_avgstat_2', 'AMKAT_avgstat_4', 'AMKAT_avgstat_8', 
      'AMKAT_avgstat_16', 'AMKAT_avgstat_32', 'AMKAT_avgstat_64', 
      'AMKAT_avgstat_128',
      'OMGA_original_Y', 'OMGA_residual_Y', 'DKAT', 'DK_L'))

candidate_kernels <- c('gau', 'lin', 'quad', 'exp')
if (x_type == 'snp') candidate_kernels <- c(candidate_kernels, 'IBS')

# Distribution parameters ------------------------------------------------------
epsilon_mean_vector <- rep(0, times = num_y_variables)
epsilon_covariance_matrix <- matrix(error_correlation_strength, 
                                    nrow = num_y_variables, 
                                    ncol = num_y_variables) 
for (i in 1:num_y_variables) {
  for (j in 1:num_y_variables) {
    if ((i + j) %% 2 == 1) {
      epsilon_covariance_matrix[i, j] <- -error_correlation_strength
    }
  }
}
diag(epsilon_covariance_matrix) <- rep(1, times = num_y_variables)    
if (error_distribution == 'cauchy') # uses precision matrix
  epsilon_precision_matrix <- 
  as.matrix(Matrix::forceSymmetric(solve(epsilon_covariance_matrix)))

if (x_type == 'cts') {
  x_mean_vector <- rep(0, times = p)
  x_covariance_matrix <- diag(p)
  for (i in 1:p) {
    for (j in i:p) x_covariance_matrix[i, j] <- 0.6 ^ (abs(i - j))
  }
  x_covariance_matrix <- as.matrix(Matrix::forceSymmetric(x_covariance_matrix))
}
if (x_type == 'snp') {
  dir_snp_reference <- system.file('examples', package = 'sim1000G')
  vcf_file <- file.path(dir_snp_reference, 'region.vcf.gz')
  vcf_reference_object <- 
    sim1000G::readVCF(vcf_file, maxNumberOfVariants = 2000, min_maf = 0.05)
}

covariate_coefficient_matrix <- 
  cbind(c(0.3, 0.1), c(0.2, -0.2), c(-0.2, -0.3), c(-0.1, 0.2))

# Signal Sets ------------------------------------------------------------------
if (x_type == 'cts') {
  if (signal_density == 'sparse') { # 7 signals
    signal_indices_shared <- c(2, 4, 6, 8)
    signal_indices_y1 <- 3
    signal_indices_y2_only <- 5
    signal_indices_y3 <- 7
    signal_indices_y4 <- 7
  }
  # if (signal_density == 'regular') { # 32 signals
  #   signal_indices_shared <- c((5:8), (13:16), (21:24), (29:32))
  #   signal_indices_y2_only <- c((3:4), (9:12) , (17:20), (25:28), (33:34))
  #   signal_indices_y1 <- c(12, 17, 20, 25)
  #   signal_indices_y3 <- c(12, 17, 4, 9, 28, 33)
  #   signal_indices_y4 <- c(20, 25, 4, 9, 28, 33)
  # }
  if (signal_density == 'dense') { # 80 signals
    signal_indices_shared <- c((5:8), (13:16), (21:24), (29:32), (37:40), 
                               (45:48), (53:56), (61:64), (69:72), (77:80))
    signal_indices_y2_only <- 
      c((3:4), (9:12), (17:20), (25:28), (33:36), (41:44), (49:52), (57:60), 
        (65:68), (73:76), (81:82))
    signal_indices_y1 <- c(28, 33, 36, 41, 44, 49, 52, 57, 60, 65)
    signal_indices_y3 <- c(28, 33, 36, 41, 44, 
                           4, 9, 12, 17, 20, 25, 68, 73, 76, 81)
    signal_indices_y4 <- c(49, 52, 57, 60, 65, 
                           4, 9, 12, 17, 20, 25, 68, 73, 76, 81)
  }
  # if (signal_density == 'hddense') { # 200 signals
  #   signal_indices_shared <- seq(from = 2, to = 200, by = 2)
  #   signal_indices_y1 <- seq(from = 3, length = 25, by = 2)
  #   signal_indices_y2_only <- seq(from = 1, to = 199, by = 2)
  #   signal_indices_y3 <- seq(from = 53, length = 25, by = 2)
  #   signal_indices_y4 <- seq(from = 27, length = 25, by = 2)
  # }
}  
if (x_type == 'snp') {
  region1 <- 132:(132 + 46)
  region2 <- 213:(213 + 30)
  region3 <- 505:(505 + 44)
  subregion1 <- region1[c(39, 41:46)]
  subregion2 <- region2[c(2:8)]
  subregion3 <- region3[c(9, 10, 11, 13, 15, 16, 17)]
  subregion4 <- region3[c(36:40, 42, 43)]
  if (signal_density == 'sparse') { # 28 signals
    signal_set <- c(subregion1, subregion2, subregion3, subregion4)
  } else { # 123 signals
    signal_set <- c(region1, region2, region3)
  }
  signal_indices_shared <- 
    sort(union(seq(from = 1, to = length(signal_set), by = 4),
               seq(from = 2, to = length(signal_set), by = 4)))
  signal_indices_y2_only <- 
    sort(union(seq(from = 3, to = length(signal_set), by = 4),
               seq(from = 4, to = length(signal_set), by = 4)))
  signal_indices_y1 <- seq(from = 3, to = length(signal_set), by = 4)
  signal_indices_y3 <- seq(from = 4, to = length(signal_set), by = 4)
  signal_indices_y4 <- signal_indices_y2_only
  if (length(signal_indices_shared) > length(signal_indices_y2_only)) {
    signal_indices_shared <- 
      signal_indices_shared[c(1:length(signal_indices_y2_only))]
  }
  if (length(signal_indices_y2_only) > length(signal_indices_shared)) {
    signal_indices_y2_only <- 
      signal_indices_y2_only[c(1:length(signal_indices_shared))]
  }
}  
signal_indices_y1 <- union(signal_indices_shared, signal_indices_y1)
signal_indices_y3 <- union(signal_indices_shared, signal_indices_y3)
signal_indices_y4 <- union(signal_indices_shared, signal_indices_y4)

# Signal strength coefficients -------------------------------------------------
if (x_type == 'cts') {
  baseline_signal_strength <- switch(signal_density,
                                     sparse = 1,      
                                     regular = 3 / 5,    
                                     dense = 1 / 3,   
                                     hddense = 1 / 4)
  strength_modifier_y1 <- 1
  strength_modifier_y2 <- switch(signal_density,
                                 sparse = 3 / 4,
                                 regular = 3 / 4,
                                 dense = 4 / 5,
                                 hddense = 4 / 5
  )
  strength_modifier_y3 <- switch(signal_density,
                                 sparse = 4 / 5,
                                 regular = 1 / 5,
                                 dense = 1 / 4,
                                 hddense = 1 / 4)
  strength_modifier_y4 <- switch(signal_density,
                                 sparse = 8,
                                 regular = 6,
                                 dense = 6,
                                 hddense = 6)
}
if (x_type == 'snp') {
  baseline_signal_strength <- switch(signal_density,
                                     sparse = 0.275,    
                                     dense = 0.04)
  strength_modifier_y1 <- switch(signal_density,
                                 sparse = 0.54,
                                 dense = 0.72)
  strength_modifier_y2 <- switch(signal_density,
                                 sparse = 0.6,
                                 dense = 0.5)
  strength_modifier_y3 <- 0.1
  strength_modifier_y4 <- 0.75
}

if (error_distribution == 'cauchy') {
  baseline_signal_strength <- 2 * baseline_signal_strength
}

overall_signal_strength <- baseline_signal_strength * signal_strength
# The value of signal_strength is set outside of this script.

# Effect functions  ------------------------------------------------------------
hermitePolyDeg2 <- function(x, scale, shape, coefficient) {
  ax2 <- (scale * x) ^ 2
  return((4 * ax2 - 2) * exp(-ax2 / shape) * coefficient)
}
hermitePolyDeg3 <- function(x, scale, shape, coefficient) {
  ax <- scale * x
  return((8 * ax ^ 3 - 12 * ax) * exp(-ax ^ 2 / shape) * coefficient)
}                   
hermitePolyDeg4 <- function(x, scale, shape, coefficient) {
  ax <- scale * x
  return((16 * ax ^ 4 - 48 * ax ^ 2 + 12) * exp(-ax ^ 2 / shape) * coefficient)
}
if (x_type == 'cts') {
  if (signal_density == 'sparse') {
    computeEffectOnY1 <- function(x, coefficient) {
      x_signals <- x[signal_indices_y1]
      return (coefficient * (x_signals[[5]] + x_signals[[1]] - x_signals[[2]] + 
                               x_signals[[3]] - x_signals[[4]]))
    }
    computeEffectOnY2 <- function(x, coefficient) {
      x_signals_shared <- x[signal_indices_shared]
      marginal_signal_for_y1 <- signal_indices_y1[[5]]
      marginal_signal_for_y3 <- signal_indices_y3[[5]]
      return(coefficient * 
               (x[[signal_indices_y2_only]]^2 + x[[signal_indices_y2_only]] + 
                  x[[signal_indices_y2_only]] * x[[marginal_signal_for_y1]] - 
                  x[[signal_indices_y2_only]] * x[[marginal_signal_for_y3]] + 
                  x[[marginal_signal_for_y3]] * x[[marginal_signal_for_y1]] + 
                  x[[marginal_signal_for_y3]] * x_signals_shared[[1]] - 
                  x_signals_shared[[2]] * x_signals_shared[[4]] + 
                  x_signals_shared[[3]]))
    }
    computeEffectOnY3 <- function(x, coefficient) {
      x_signals <- x[signal_indices_y3]
      return(coefficient * 
               (hermitePolyDeg2(x_signals[[2]], scale = 3 / 4, shape = 1, 
                                coefficient = 1) +
                  hermitePolyDeg3(x_signals[[5]], scale = 3 / 4, shape = 1, 
                                  coefficient = 0.5) +
                  hermitePolyDeg3(x_signals[[1]], scale = 3 / 4, shape = 1, 
                                  coefficient = 0.5) +
                  hermitePolyDeg4(x_signals[[3]], scale = 3 / 4, shape = 1, 
                                  coefficient = -0.25) +
                  hermitePolyDeg4(x_signals[[4]], scale = 3 / 4, shape = 1, 
                                  coefficient = -0.25)))
    }
    computeEffectOnY4 <- function(x, coefficient) {
      x_signals <- x[signal_indices_y4]
      return(coefficient * sum(cos(x_signals) * exp(-x_signals ^ 2 / 10)))
    }
  }
  if (signal_density == 'dense') {
    alternating_signs <- rep(1, times = length(signal_indices_y1))
    for (i in seq(from = 2, to = length(signal_indices_y1), by = 2)) {
      alternating_signs[[i]] <- -1
    }
    computeEffectOnY1 <- function(x, coefficient) {
      return(coefficient * sum(alternating_signs * x[signal_indices_y1]))
    }
    computeEffectOnY2 <- function(x, coefficient) {
      return(coefficient * 
               sum(x[signal_indices_shared] * x[signal_indices_y2_only]))
    }
    computeEffectOnY3 <- function(x, coefficient) {
      x_signals <- 0.7 * x[signal_indices_y3]
      return(coefficient * sum((8 * x_signals ^ 3 - 12 * x_signals) * 
                                 exp(-x_signals ^ 2 / 3)))
    }
    computeEffectOnY4 <- function(x, coefficient) {
      x_signals <- x[signal_indices_y4]
      return(coefficient * sum(cos(x_signals) * exp(-x_signals ^ 2 / 10)))
    }          
  }
}
if (x_type == 'snp') {
  computeEffectOnY1 <- function(x, coefficient) {
    return(coefficient * sum(x[signal_indices_y1]))
  }
  computeEffectOnY2 <- function(x, coefficient) {
    return(coefficient * 
             sum(x[signal_indices_shared] * x[signal_indices_y2_only]))
  }
  computeEffectOnY3 <- function(x, coefficient) {
    x_signals <- 0.7 * x[signal_indices_y3]
    return(coefficient * sum((8 * x_signals ^ 3 - 12 * x_signals) * 
                               exp(-x_signals ^ 2 / 3)))
  }
  computeEffectOnY4 <- function(x, coefficient) {
    x_signals <- x[signal_indices_y4]
    return(coefficient * sum(cos(x_signals) * exp(-x_signals ^ 2 / 10)))
  }
}

computeEffectOnY <- function(x) {
  out <- matrix(NA, nrow = n, ncol = num_y_variables) 
  for (i in seq_len(n)) { 
    out[i, 1] <- 
      computeEffectOnY1(x[i, ], overall_signal_strength * strength_modifier_y1)     
    out[i, 2] <- 
      computeEffectOnY2(x[i, ], overall_signal_strength * strength_modifier_y2)     
    out[i, 3] <- 
      computeEffectOnY3(x[i, ], overall_signal_strength * strength_modifier_y3)     
    out[i, 4] <- 
      computeEffectOnY4(x[i, ], overall_signal_strength * strength_modifier_y4)          
  }
  return(out)   
}

# Functions to simulate data ---------------------------------------------------
if (x_type == 'cts') {
  simulateDataX <- function(){
    return(mvrnorm(n, x_mean_vector, x_covariance_matrix))
  }
}
if (x_type == 'snp') {
  simulateDataX <- function() {
    sim1000G::startSimulation(vcf_reference_object, 
                              totalNumberOfIndividuals = 1000)
    subject_ids <- sim1000G::generateUnrelatedIndividuals(n)
    return(sim1000G::retrieveGenotypes(subject_ids))
  }
}
simulateDataCovariates <- function() {
  covariates_column1 <- rnorm(n)
  covariates_column2 <- rbinom(n, size = 1, p = 0.4)
  return(cbind(covariates_column1, covariates_column2))
}
if (error_distribution == 'cauchy') {
  simulateDataEpsilon <- function() {
    return(LaplacesDemon::rmvcp(n, epsilon_mean_vector, 
                                epsilon_precision_matrix))
  }
}
if (error_distribution == 'normal') {
  simulateDataEpsilon <- function() {
    return(MASS::mvrnorm(n, epsilon_mean_vector, epsilon_covariance_matrix))
  }
}
if (size_or_power == 'size') {
  simulateDataY <- function(x, covariates) {
    covariate_effects_on_y <- covariates %*% covariate_coefficient_matrix
    epsilon <- simulateDataEpsilon()
    return(covariate_effects_on_y + epsilon)
  }
}
if (size_or_power == 'power') {
  simulateDataY <- function(x, covariates) {
    x_effects_on_y <- computeEffectOnY(x)
    covariate_effects_on_y <- covariates %*% covariate_coefficient_matrix
    epsilon <- simulateDataEpsilon()
    return(covariate_effects_on_y + x_effects_on_y + epsilon)             
  }
}