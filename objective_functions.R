library(tidyverse)

# Load simulation cube
sim_cube <- readRDS("simulated_forecast_cube.rds")

n_months <- dim(sim_cube)[1]
n_assets <- dim(sim_cube)[2]
n_simulations <- dim(sim_cube)[3]

# ---- CONFIG ----
drawdown_limit <- 0.05      # 5% max drawdown
penalty_factor <- 1000      # Weight of drawdown penalty

# Function to compute portfolio returns from weights and sim cube
portfolio_returns_from_weights <- function(weights, cube) {
  # For each simulation, multiply asset returns by weights, sum across assets for each month
  portfolio_returns <- sapply(1:n_simulations, function(sim) {
    # Multiply each asset's returns by weight and sum across assets per month
    rowSums(cube[,,sim] * matrix(weights, nrow = n_months, ncol = n_assets, byrow = TRUE))
  })
  return(portfolio_returns)
}

# Function to compute max drawdown for each simulation
max_drawdown <- function(ret_vec) {
  cumulative <- cumprod(1 + ret_vec)
  max_dd <- max(1 - cumulative / cummax(cumulative))
  return(max_dd)
}

# ---- OBJECTIVE FUNCTION ----
# Returns negative median portfolio return (compounded) + drawdown penalty
objective_function <- function(weights) {
  # Enforce weights constraints: no shorts, sum to 1 (tolerance 1e-4)
  if (any(weights < 0) || abs(sum(weights) - 1) > 1e-4) return(Inf)
  
  port_returns_matrix <- portfolio_returns_from_weights(weights, sim_cube)  # [months, trials]
  
  # Compound returns across months for each simulation trial
  final_returns <- apply(port_returns_matrix, 2, function(x) prod(1 + x) - 1)
  
  median_return <- median(final_returns)
  
  # Calculate max drawdown across trials
  drawdowns <- apply(port_returns_matrix, 2, max_drawdown)
  
  # Penalty if drawdown limit exceeded
  dd_violations <- drawdowns[drawdowns > drawdown_limit]
  penalty <- if (length(dd_violations) > 0) {
    mean(dd_violations - drawdown_limit) * penalty_factor
  } else {
    0
  }
  
  # Objective = minimize negative median return + penalty
  return(-median_return + penalty)
}

# Example usage: equal weights
w <- rep(1 / n_assets, n_assets)
objective_function(w)
