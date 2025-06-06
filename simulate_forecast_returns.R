# simulate_forecast_returns.R

library(tidyverse)

# Load the synthetic returns
returns_df <- read_csv("synthetic_returns.csv")
returns_mat <- returns_df %>% select(-Month) %>% as.matrix()

n_assets <- ncol(returns_mat)
n_months <- 12       # forecast horizon
n_simulations <- 500 # number of Monte Carlo trials
block_size <- 3      # block length for bootstrap

# Forecast views per strategy
forecast_views <- tibble(
  strategy = colnames(returns_mat),
  optimistic = c(0.20, 0.25, 0.18, 0.22, 0.28, 0.23, 0.20, 0.21),
  most_likely = c(0.10, 0.12, 0.09, 0.11, 0.13, 0.10, 0.11, 0.10),
  pessimistic = c(-0.05, -0.06, -0.04, -0.05, -0.07, -0.06, -0.05, -0.04),
  prob_optimistic = 0.25,
  prob_most_likely = 0.50,
  prob_pessimistic = 0.25
)

# Helper function to apply forecast view to a sample
apply_forecast_shift <- function(returns, shift) {
  return(returns + shift)
}

# Create simulation cube: [months, assets, trials]
sim_cube <- array(NA, dim = c(n_months, n_assets, n_simulations))

# Simulation loop
for (sim in 1:n_simulations) {
  for (a in 1:n_assets) {
    # Sample blocks
    block_starts <- sample(1:(nrow(returns_mat) - block_size + 1), size = ceiling(n_months / block_size), replace = TRUE)
    boot_sample <- unlist(lapply(block_starts, function(start) returns_mat[start:(start + block_size - 1), a]))
    boot_sample <- boot_sample[1:n_months]  # Trim extra if needed
    
    # Assign forecast shift based on probability
    probs <- c(forecast_views$prob_optimistic[a], 
               forecast_views$prob_pessimistic[a], 
               forecast_views$prob_most_likely[a])
    view_shift <- sample(
      x = c(forecast_views$optimistic[a], forecast_views$pessimistic[a], forecast_views$most_likely[a]),
      size = 1,
      prob = probs
    )
    
    shifted_returns <- apply_forecast_shift(boot_sample, view_shift)
    sim_cube[, a, sim] <- shifted_returns
  }
}

# Save simulation cube
saveRDS(sim_cube, file = "simulated_forecast_cube.rds")

print("✅Monte Carlo simulations with forecast views completed and saved.")


