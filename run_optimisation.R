# run_optimisation.R

 install.packages("GenSA")

library(GenSA)

# Load your objective function
source("objective_functions.R")

# ---- Optimisation Setup ----

n_assets <- dim(sim_cube)[2]

# Bounds for each asset
lower_bounds <- rep(0.0, n_assets)
upper_bounds <- rep(0.25, n_assets)  # No asset gets more than 25%
budget_constraint <- 1.0

# Constraint wrapper
constrained_objective <- function(weights) {
  weights <- weights / sum(weights)  # Normalize to sum to 1
  return(objective_function(weights))
}

# Run GenSA
set.seed(42)

result <- GenSA(
  par = rep(1 / n_assets, n_assets),       # Initial guess
  fn = constrained_objective,
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(
    max.call = 5000,
    verbose = TRUE
  )
)

# Final normalised weights
final_weights <- result$par / sum(result$par)

# ---- Optional: Save results ----
saveRDS(final_weights, "optimal_weights.rds")

# ---- Optional: Print results ----
cat("✅ Optimisation complete.\n")
cat("Optimal weights:\n")
print(round(final_weights, 4))

# Optional: Final objective value
cat("✅ Final Objective Value:", result$value, "\n")

# Optional: Check that weights sum to 1
cat("Sum of weights:", sum(final_weights), "\n")

# Optional: Visualize weights
barplot(final_weights,
        names.arg = colnames(sim_cube[,,1]),
        las = 2,
        main = "Optimal Portfolio Weights",
        col = "steelblue")
write_csv(tibble(Strategy = colnames(sim_cube[,,1]), Weight = round(final_weights, 4)),
          "optimal_weights.csv")


