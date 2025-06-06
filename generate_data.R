# generate_data.R

# Load libraries
library(tidyverse)

# Settings
set.seed(123)  # for reproducibility

n_assets <- 8
n_months <- 60

# Define strategy names
strategy_names <- c(
  "Equity_Market_Neutral", "Equity_Long_Short", "Fixed_Income_Arbitrage",
  "Macro", "Managed_Futures", "Credit", 
  "Mergers_Special_Situations", "Multi_Strategy"
)

# Parameters for each strategy
strategy_params <- tibble(
  strategy = strategy_names,
  mean = c(0.005, 0.008, 0.006, 0.007, 0.006, 0.009, 0.005, 0.007),
  sd = c(0.02, 0.03, 0.015, 0.025, 0.035, 0.03, 0.025, 0.02),
  ar1 = c(0.2, 0.3, 0.1, 0.2, 0.25, 0.3, 0.2, 0.2),
  skew = c("left", "right", "none", "none", "right", "left", "right", "mixed")
)

# Function to simulate AR(1) series
simulate_ar1_series <- function(n, mu, sigma, phi, skew_type) {
  e <- rnorm(n, 0, sigma)
  y <- numeric(n)
  y[1] <- mu + e[1]
  for (t in 2:n) {
    y[t] <- mu + phi * (y[t - 1] - mu) + e[t]
  }
  # Apply skew transformation
  if (skew_type == "left") {
    y <- y - abs(rnorm(n, 0, sigma / 3))
  } else if (skew_type == "right") {
    y <- y + abs(rnorm(n, 0, sigma / 3))
  } else if (skew_type == "mixed") {
    y <- y + rnorm(n, 0, sigma / 4) * sample(c(-1, 1), n, replace = TRUE)
  }
  return(y)
}

# Simulate all strategies
returns_matrix <- map_dfc(1:n_assets, function(i) {
  p <- strategy_params[i, ]
  sim_returns <- simulate_ar1_series(
    n = n_months,
    mu = p$mean[[1]],
    sigma = p$sd[[1]],
    phi = p$ar1[[1]],
    skew_type = p$skew[[1]]
  )
  tibble(!!p$strategy[[1]] := sim_returns)
})

# Add month index
returns_df <- returns_matrix %>%
  mutate(Month = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = n_months)) %>%
  relocate(Month)

# Save to CSV
write_csv(returns_df, "synthetic_returns.csv")

# Done
print("synthetic hedge fund data saved as synthetic_returns.csv")

df <- read_csv("synthetic_returns.csv")
glimpse(df)
summary(df)

library(ggplot2)

df_long <- df %>% pivot_longer(-Month, names_to = "Strategy", values_to = "Return")

ggplot(df_long, aes(x = Month, y = Return, color = Strategy)) +
  geom_line(size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Simulated Monthly Returns of Hedge Fund Strategies",
       x = "Month",
       y = "Return") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(title = "Simulated Monthly Returns of Hedge Fund Strategies")
