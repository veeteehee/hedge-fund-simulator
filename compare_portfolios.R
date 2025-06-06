# compare_portfolios.R

library(tidyverse)

# ---- Load Data ----
sim_cube <- readRDS("simulated_forecast_cube.rds")
final_weights <- readRDS("optimal_weights.rds")

n_assets <- dim(sim_cube)[2]
n_simulations <- dim(sim_cube)[3]
n_months <- dim(sim_cube)[1]

equal_weights <- rep(1 / n_assets, n_assets)

# ---- Function to Calculate Portfolio Returns ----
portfolio_returns <- function(weights, cube) {
  sapply(1:n_simulations, function(sim) {
    rowSums(t(t(cube[,,sim]) * weights))
  })  # [months x simulations]
}

# ---- Generate Portfolio Return Matrices ----
opt_returns <- portfolio_returns(final_weights, sim_cube)
eq_returns <- portfolio_returns(equal_weights, sim_cube)

# ---- Collapse to Final Return + Drawdown Stats ----
calc_drawdown <- function(r) {
  cum <- cumprod(1 + r)
  max(1 - cum / cummax(cum))
}

get_stats <- function(ret_matrix, label) {
  tibble(
    sim = 1:n_simulations,
    final_return = colSums(ret_matrix),
    max_dd = apply(ret_matrix, 2, calc_drawdown),
    strategy = label
  )
}

df_opt <- get_stats(opt_returns, "Optimised")
df_eq  <- get_stats(eq_returns, "Equal Weight")
df_all <- bind_rows(df_opt, df_eq)

# ---- Performance Ratios ----
get_ratios <- function(ret_matrix, label) {
  ratio_df <- tibble(
    sim = 1:n_simulations,
    mean_return = colMeans(ret_matrix),
    sd_return = apply(ret_matrix, 2, sd),
    final_return = colSums(ret_matrix),
    max_dd = apply(ret_matrix, 2, calc_drawdown),
    strategy = label
  ) %>%
    mutate(
      sharpe = ifelse(sd_return > 0, mean_return / sd_return, NA),
      calmar = ifelse(max_dd > 0, mean_return / max_dd, NA)
    )
  return(ratio_df)
}

ratios_opt <- get_ratios(opt_returns, "Optimised")
ratios_eq  <- get_ratios(eq_returns, "Equal Weight")
ratios_all <- bind_rows(ratios_opt, ratios_eq)

# ---- Summary of Ratios ----
summary_ratios <- ratios_all %>%
  group_by(strategy) %>%
  summarise(
    avg_sharpe = mean(sharpe, na.rm = TRUE),
    avg_calmar = mean(calmar, na.rm = TRUE),
    .groups = "drop"
  )

cat("📈 Performance Ratios Summary:\n")
print(summary_ratios)

# ---- Summary Stats ----
summary_stats <- df_all %>%
  group_by(strategy) %>%
  summarise(
    median_return = median(final_return),
    mean_return = mean(final_return),
    sd_return = sd(final_return),
    median_max_dd = median(max_dd),
    .groups = "drop"
  )
print(summary_stats)

# ---- Plots ----

# 1. Final Return Distribution
ggplot(df_all, aes(x = final_return, fill = strategy)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Final Portfolio Return Distribution",
       x = "Cumulative Return", y = "Density") +
  scale_fill_manual(values = c("Optimised" = "#2c7bb6", "Equal Weight" = "#d7191c"))

# 2. Max Drawdown Distribution
ggplot(df_all, aes(x = max_dd, fill = strategy)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Maximum Drawdown Distribution",
       x = "Max Drawdown", y = "Density") +
  scale_fill_manual(values = c("Optimised" = "#2c7bb6", "Equal Weight" = "#d7191c"))

# 3. Median Cumulative Paths
cum_paths <- function(ret_matrix) {
  apply(1 + ret_matrix, 2, cumprod)
}

opt_paths <- cum_paths(opt_returns)
eq_paths <- cum_paths(eq_returns)

med_path <- tibble(
  Month = 1:n_months,
  Optimised = apply(opt_paths, 1, median),
  Equal = apply(eq_paths, 1, median)
) %>%
  pivot_longer(-Month, names_to = "Strategy", values_to = "CumulativeReturn")

ggplot(med_path, aes(x = Month, y = CumulativeReturn, color = Strategy)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Median Cumulative Return Path", y = "Growth of $1") +
  scale_color_manual(values = c("Optimised" = "#2c7bb6", "Equal" = "#d7191c"))







