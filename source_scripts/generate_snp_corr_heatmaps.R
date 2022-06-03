# Define correlation heatmap function
heatmap <- function(X){
  ggplot(data = melt(round(cor(X), 2)),
         aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkblue",
                         high = "red",
                         mid = "white", midpoint = 0, limit = c(-1,1),
                         space = "Lab",
                         name = "Pearson correlation coefficient") +
    coord_fixed() +
    theme_classic() +
    theme(legend.title = element_text(size = 10),
          legend.position = "bottom") +
    labs(x = "SNP set index", y = "SNP set index",
         # title = "Correlation Heatmap",
         # subtitle = paste0(tag, " (", ncol(X), " SNPs)")
    )
}



# Full SNP set ------------------------------------------------------------


# Scenario parameters
size_or_power <- "power"
alpha <- 0.05
num_permutations <- 1000
num_replicates <- 1000
x_type <- "snp"
error_distribution <- "normal"
signal_strength <- 1
signal_density <- "sparse"
signal_correlation <- "low"
error_correlation_strength <- 0.5
n <- 1000
p <- 567

# Initialize scenario simulation environment
source(file.path(dir_src, 'initialize_simulation_scenario.R'))

# Simulate data
X <- simulateDataX()

# Generate correlation heatmap
filename <- file.path(dir_plots_snp_corr_heatmaps, "full_snpset.pdf")
grDevices::pdf(file = filename)
heatmap(X)
grDevices::dev.off()



# Signal sets -------------------------------------------------------------


### WEAKLY-CORRELATED SIGNAL VARIABLES ###

# 28-variable signal
all_signals <- sort(unique(c(
  signal_indices_shared, signal_indices_y1, signal_indices_y2_only,
  signal_indices_y3, signal_indices_y4)))
filename <- file.path(dir_plots_snp_corr_heatmaps, "weak_28.pdf")
grDevices::pdf(file = filename)
heatmap(X[ , all_signals]) + labs(x = "signal index", y = "signal index")
grDevices::dev.off()


# 122-variable signal
signal_density <- "dense"
source(file.path(dir_src, 'initialize_simulation_scenario.R'))
X <- simulateDataX()
all_signals <- sort(unique(c(
  signal_indices_shared, signal_indices_y1, signal_indices_y2_only,
  signal_indices_y3, signal_indices_y4)))
filename <- file.path(dir_plots_snp_corr_heatmaps, "weak_122.pdf")
grDevices::pdf(file = filename)
heatmap(X[ , all_signals]) + labs(x = "signal index", y = "signal index")
grDevices::dev.off()



### STRONGLY-CORRELATED SIGNAL VARIABLES ###

# 28-variable signal
signal_correlation <- "high"
signal_density <- "sparse"
source(file.path(dir_src, 'initialize_simulation_scenario.R'))
X <- simulateDataX()
all_signals <- sort(unique(c(
  signal_indices_shared, signal_indices_y1, signal_indices_y2_only,
  signal_indices_y3, signal_indices_y4)))
filename <- file.path(dir_plots_snp_corr_heatmaps, "strong_28.pdf")
grDevices::pdf(file = filename)
heatmap(X[ , all_signals]) + labs(x = "signal index", y = "signal index")
grDevices::dev.off()


# 122-variable signal
signal_density <- "dense"
source(file.path(dir_src, 'initialize_simulation_scenario.R'))
X <- simulateDataX()
all_signals <- sort(unique(c(
  signal_indices_shared, signal_indices_y1, signal_indices_y2_only,
  signal_indices_y3, signal_indices_y4)))
filename <- file.path(dir_plots_snp_corr_heatmaps, "strong_122.pdf")
grDevices::pdf(file = filename)
heatmap(X[ , all_signals]) + labs(x = "signal index", y = "signal index")
grDevices::dev.off()

