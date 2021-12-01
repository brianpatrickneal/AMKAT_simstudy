# Generate AMKAT P-values -------------------------------------------------------
pvalues_amkat_no_filter <- pvalues_amkat <-
  pvals_amkat_mpv_100x10perms <- pvals_amkat_mpv_50x20perms <-
  pvals_amkat_mpv_20x50perms <- pvals_amkat_mpv_10x100perms <-
  pvals_amkat_avgstat_10 <- pvals_amkat_avgstat_20 <-
  pvals_amkat_avgstat_50 <- pvals_amkat_avgstat_100 <-
  rep(NA, times = 2 * num_replicates)

# Using pseudocount adjustment to p-value
for (i in seq_len(num_replicates)) {
  pvalues_amkat_no_filter[[i]] <- mean(
    test_stat_no_filter <= permutation_stats_no_filter[1:num_permutations, i]
  ) + pseudocount
  pvalues_amkat[[i]] <- mean(
    observed_statistics[1, i] <= permutation_stats[1:num_permutations, i]
  ) + pseudocount
  pvals_amkat_mpv_100x10perms[[i]] <- mean(generatePValues(
    observed_statistics[1:100, i], permutation_stats[1:num_permutations, i])
  ) + pseudocount
  pvals_amkat_mpv_50x20perms[[i]] <- mean(generatePValues(
    observed_statistics[1:50, i], permutation_stats[1:num_permutations, i])
  ) + pseudocount
  pvals_amkat_mpv_20x50perms[[i]] <- mean(generatePValues(
    observed_statistics[1:20, i], permutation_stats[1:num_permutations, i])
  ) + pseudocount
  pvals_amkat_mpv_10x100perms[[i]] <- mean(generatePValues(
    observed_statistics[1:10, i], permutation_stats[1:num_permutations, i])
  ) + pseudocount
  pvals_amkat_avgstat_10[[i]] <-
    mean(mean(observed_statistics[1:10, i]) <=
           permutation_stats[1:num_permutations, i]) + pseudocount
  pvals_amkat_avgstat_20[[i]] <-
    mean(mean(observed_statistics[1:20, i]) <=
           permutation_stats[1:num_permutations, i]) + pseudocount
  pvals_amkat_avgstat_50[[i]] <-
    mean(mean(observed_statistics[1:50, i]) <=
           permutation_stats[1:num_permutations, i]) + pseudocount
  pvals_amkat_avgstat_100[[i]] <-
    mean(mean(observed_statistics[1:100, i]) <=
           permutation_stats[1:num_permutations, i]) + pseudocount
}
# Using floor adjustment to p-value
for (i in seq_len(num_replicates)) {
  pvalues_amkat_no_filter[[i + num_replicates]] <- max(mean(
    test_stat_no_filter <= permutation_stats_no_filter[1:num_permutations, i]),
    pseudocount)
  pvalues_amkat[[i + num_replicates]] <- max(
    mean(observed_statistics[1, i] <= permutation_stats[1:num_permutations, i]),
    pseudocount)
  pvals_amkat_mpv_100x10perms[[i + num_replicates]] <- max(
    mean(generatePValues(observed_statistics[1:100, i],
                         permutation_stats[1:num_permutations, i])),
    pseudocount)
  pvals_amkat_mpv_50x20perms[[i + num_replicates]] <- max(
    mean(generatePValues(observed_statistics[1:50, i],
                         permutation_stats[1:num_permutations, i])),
    pseudocount)
  pvals_amkat_mpv_20x50perms[[i + num_replicates]] <- max(
    mean(generatePValues(observed_statistics[1:20, i],
                         permutation_stats[1:num_permutations, i])),
    pseudocount)
  pvals_amkat_mpv_10x100perms[[i + num_replicates]] <- max(
    mean(generatePValues(observed_statistics[1:10, i],
                         permutation_stats[1:num_permutations, i])),
    pseudocount)
  pvals_amkat_avgstat_10[[i + num_replicates]] <-
    max(mean(mean(observed_statistics[1:10, i]) <=
               permutation_stats[1:num_permutations, i]), pseudocount)
  pvals_amkat_avgstat_20[[i + num_replicates]] <-
    max(mean(mean(observed_statistics[1:20, i]) <=
               permutation_stats[1:num_permutations, i]), pseudocount)
  pvals_amkat_avgstat_50[[i + num_replicates]] <-
    max(mean(mean(observed_statistics[1:50, i]) <=
               permutation_stats[1:num_permutations, i]), pseudocount)
  pvals_amkat_avgstat_100[[i + num_replicates]] <-
    max(mean(mean(observed_statistics[1:100, i]) <=
               permutation_stats[1:num_permutations, i]), pseudocount)
}
pvalues <-
  data.frame(pvalues_amkat_no_filter, pvalues_amkat, pvals_amkat_avgstat_10,
             # pvals_amkat_avgstat_20, pvals_amkat_avgstat_50,
             pvals_amkat_avgstat_100, pvals_amkat_mpv_10x100perms,
             # pvals_amkat_mpv_20x50perms,  pvals_amkat_mpv_50x20perms,
             pvals_amkat_mpv_100x10perms)
colnames(pvalues) <- method_labels
pvalues_pseudo <- pvalues[1:num_replicates, ]
pvalues_floor <- pvalues[((1:num_replicates) + num_replicates), ]
pvalues_avgstat <- data.frame(pvalues_amkat, pvals_amkat_avgstat_10,
                              pvals_amkat_avgstat_20, pvals_amkat_avgstat_50,
                              pvals_amkat_avgstat_100)
colnames(pvalues_avgstat) <- method_labels_avgstat
pvalues_avgstat <- pvalues_avgstat[1:num_replicates, ] # use pseudocount