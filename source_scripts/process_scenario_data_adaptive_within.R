### Script: Generate P-values using simulated statistics ###

# create storage array for P-values
col_labels <- c('NF', paste0('PF-', Q_values))
row_labels <- as.character(B_values)
pvalues <-
  array(2, dim = c(length(row_labels), length(col_labels), num_replicates),
        dimnames = list(row_labels, col_labels, NULL))

col_values <- c(0, Q_values)
row_values <- B_values

# Compute sample of P-values for each variation of AMKAT
for (i in seq_len(num_replicates)) {
  for (q in seq_along(col_labels)) {

    Q <- col_values[[q]] # number of test stats

    for (b in seq_along(row_labels)) {

      B <- row_values[[b]] # number of permutation stats

      if (Q == 0) { # no filter
        pvalues[b, q, i] <-
          mean(test_stat_no_filter <= perm_stats_no_filter[1:B, i]) + (1 / B)
      } else { # PhiMr filter
        pvalues[b, q, i] <-
          mean(mean(test_stats[1:Q, i]) <= perm_stats[1:B, i]) + (1 / B)
      }
    }
  }
}
