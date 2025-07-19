# created July 19, 2025

library(tidyverse)
library(fixest)
library(ivreg)
library(broom)
library(ggplot2)
library(knitr)

# R Implementation ----
# Set seed for reproducibility
set.seed(123)

# Generate synthetic e-commerce data
generate_ecommerce_data <- function(n = 5000) {
  # Create panel structure
  regions <- c("Northeast", "Southeast", "Midwest", "West", "Southwest")
  months <- 1:24

  # Expand grid for panel
  data <- tidyr::expand_grid(
    region = regions,
    month = months,
    customer_id = 1:200  # 200 customers per region-month
  ) %>%
    dplyr::slice_sample(n = n) %>%
    dplyr::mutate(
      # Geographic factors affecting shipping
      distance_to_hub = case_when(
        region == "Northeast" ~ 500,
        region == "Southeast" ~ 800,
        region == "Midwest" ~ 300,
        region == "West" ~ 1200,
        region == "Southwest" ~ 900
      ),

      # Time-varying factors
      fuel_price = 3 + 0.5 * sin(2 * pi * month / 12) + rnorm(n(), 0, 0.2),
      seasonal_demand = 1 + 0.3 * cos(2 * pi * month / 12),

      # Unobserved demand shock (creates endogeneity)
      demand_shock = rnorm(n(), 0, 0.3),

      # Shipping cost (our instrument)
      shipping_cost = 5 + 0.002 * distance_to_hub + 2 * fuel_price + rnorm(n(), 0, 1),

      # Product price (endogenous - responds to demand)
      base_price = 50,
      price = base_price + 5 * seasonal_demand + 3 * demand_shock +
        0.5 * shipping_cost + rnorm(n(), 0, 2),

      # Total price paid by consumer
      total_price = price + shipping_cost,

      # Quantity demanded (true elasticity = -1.5)
      log_quantity = 4 - 1.5 * log(total_price) + 0.5 * seasonal_demand +
        demand_shock + rnorm(n(), 0, 0.2),
      quantity = exp(log_quantity),

      # Revenue
      revenue = price * quantity,

      # Add customer characteristics
      income = exp(rnorm(n(), 10.5, 0.3)),
      age = sample(18:65, n(), replace = TRUE)
    ) %>%
    # Create some missing values to make realistic
    dplyr::mutate(
      price = ifelse(runif(n()) < 0.02, NA, price),
      quantity = ifelse(quantity < 0.1, 0.1, quantity)  # No negative quantities
    ) %>%
    dplyr::filter(!is.na(price))

  return(data)
}

# Generate the dataset
ecom_data <- generate_ecommerce_data(n = 5000)

# Display first few rows
head(ecom_data) %>%
  select(region, month, price, shipping_cost, total_price, quantity, revenue) %>%
  kable(digits = 2, caption = "Sample E-commerce Data")


# Exploratory Data Analysis ----
# Summary statistics
summary_stats <- ecom_data %>%
  dplyr::summarise(
    across(c(price, shipping_cost, total_price, quantity, revenue),
           list(mean = mean, sd = sd, min = min, max = max),
           .names = "{.col}_{.fn}")
  ) %>%
  tidyr::pivot_longer(everything(),
               names_to = c("variable", "statistic"),
               names_sep = "_",
               values_to = "value") %>%
  tidyr::pivot_wider(names_from = statistic, values_from = value)

print(summary_stats)

# Visualize price and quantity relationship
p1 <- ggplot(ecom_data, aes(x = log(total_price), y = log(quantity))) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Raw Price-Quantity Relationship",
       subtitle = "This relationship is biased due to endogeneity",
       x = "Log(Total Price)", y = "Log(Quantity)") +
  theme_minimal()

# Visualize instrument relevance
p2 <- ggplot(ecom_data, aes(x = shipping_cost, y = total_price)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Instrument Relevance: Shipping Cost vs Total Price",
       subtitle = "First stage relationship",
       x = "Shipping Cost", y = "Total Price") +
  theme_minimal()

# Display plots
print(p1)
print(p2)

# Estimation Strategy ----

# @@@@
# Naive OLS regression (biased due to endogeneity)
naive_model <- lm(log(quantity) ~ log(total_price) + factor(region) + factor(month) +
                    log(income) + age, data = ecom_data)

naive_results <- broom::tidy(naive_model) %>%
  dplyr::filter(term == "log(total_price)") %>%
  dplyr::mutate(method = "Naive OLS")

print("Naive OLS Results:")
print(naive_results)

# @@@@
# First stage: Total price on shipping cost
first_stage <- fixest::feols(log(total_price) ~ shipping_cost + factor(region) + factor(month) +
                       log(income) + age, data = ecom_data)

first_stage_lm <- lm(log(total_price) ~ shipping_cost + factor(region) + factor(month) +
                               log(income) + age, data = ecom_data)

# Check instrument strength
first_stage_summary <- summary(first_stage)
first_stage_summary_lm <- summary(first_stage_lm)
#f_stat <- first_stage_summary$fstatistic["shipping_cost"]
f_stat <- first_stage_summary_lm$fstatistic["value"]

print("First Stage Results:")
print(broom::tidy(first_stage_lm) %>% dplyr::filter(term == "shipping_cost"))
print(paste("F-statistic on excluded instrument:", round(f_stat, 2)))

# Rule of thumb: F > 10 for strong instrument
if(f_stat > 10) {
  print("✓ Strong instrument (F > 10)")
} else {
  print("⚠ Weak instrument (F < 10)")
}

# Two-Stage Least Squares (2SLS) ----
# 2SLS estimation using ivreg
iv_model <- ivreg::ivreg(log(quantity) ~ log(total_price) + factor(region) + factor(month) +
                    log(income) + age | shipping_cost + factor(region) + factor(month) +
                    log(income) + age, data = ecom_data)

iv_results <- broom::tidy(iv_model) %>%
  dplyr::filter(term == "log(total_price)") %>%
  dplyr::mutate(method = "2SLS")

print("2SLS Results:")
print(iv_results)

# Alternative using fixest (often more robust)
iv_model_feols <- fixest::feols(log(quantity) ~ factor(region) + factor(month) + log(income) + age |
                          log(total_price) ~ shipping_cost, data = ecom_data)

lm(log(quantity) ~ log(total_price) + factor(region) + factor(month) + log(income) + age, data = ecom_data) |>
  broom::tidy() %>%
  dplyr::filter(str_detect(term, "total_price"))

lm(log(quantity) ~ shipping_cost + factor(region) + factor(month) +
     log(income) + age, data = ecom_data) |>
  broom::tidy() %>%
  dplyr::filter(str_detect(term, "shipping_cost"))

lm(log(total_price) ~ shipping_cost, data = ecom_data) |>
  broom::tidy() %>%
  dplyr::filter(str_detect(term, "shipping_cost"))

lm(log(quantity) ~ shipping_cost, data = ecom_data) |>
  broom::tidy()

print("2SLS Results (fixest):")
print(broom::tidy(iv_model_feols) %>% dplyr::filter(str_detect(term, "total_price")))


# Compare Estimates ----
# Combine results for comparison
comparison <- dplyr::bind_rows(
  naive_results %>% dplyr::select(method, estimate, std.error, p.value),
  iv_results %>% dplyr::select(method, estimate, std.error, p.value)
) %>%
  dplyr::mutate(
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error,
    elasticity = round(estimate, 3),
    ci = paste0("[", round(ci_lower, 3), ", ", round(ci_upper, 3), "]")
  )

print("Comparison of Price Elasticity Estimates:")
print(comparison %>% dplyr::select(method, elasticity, ci))

# Visualization of estimates
ggplot(comparison, aes(x = method, y = estimate, color = method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Price Elasticity Estimates Comparison",
       subtitle = "Red line shows true elasticity (-1.5)",
       y = "Price Elasticity",
       x = "Estimation Method") +
  theme_minimal() +
  theme(legend.position = "none")

# Business Application: Revenue Optimization ----

# Function to calculate optimal price given elasticity
calculate_optimal_price <- function(elasticity, marginal_cost = 20) {
  if(elasticity >= -1) {
    return("Inelastic demand - no finite optimum")
  }
  optimal_markup <- 1 / (1 + elasticity)
  optimal_price <- marginal_cost / (1 + elasticity)
  return(list(markup = optimal_markup, price = optimal_price))
}

# Current average price and revenue
current_stats <- ecom_data %>%
  dplyr::summarise(
    avg_price = mean(price, na.rm = TRUE),
    avg_revenue = mean(revenue, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE)
  )

print("Current Business Metrics:")
print(current_stats)

# Calculate optimal prices using different elasticity estimates
elasticity_naive <- naive_results$estimate
elasticity_iv <- iv_results$estimate

optimal_naive <- calculate_optimal_price(elasticity_naive)
optimal_iv <- calculate_optimal_price(elasticity_iv)

print("Optimal Pricing Analysis:")
print(paste("Using Naive Elasticity (", round(elasticity_naive, 3), "):"))
if(is.list(optimal_naive)) {
  print(paste("  Optimal Price: $", round(optimal_naive$price, 2)))
  print(paste("  Markup: ", round(optimal_naive$markup * 100, 1), "%"))
} else {
  print(paste("  ", optimal_naive))
}

print(paste("Using IV Elasticity (", round(elasticity_iv, 3), "):"))
if(is.list(optimal_iv)) {
  print(paste("  Optimal Price: $", round(optimal_iv$price, 2)))
  print(paste("  Markup: ", round(optimal_iv$markup * 100, 1), "%"))
} else {
  print(paste("  ", optimal_iv))
}

# Revenue Simulation at Different Prices ----

# Function to simulate revenue at different price points
simulate_revenue <- function(base_data, price_range, elasticity) {
  results <- tibble()

  for(new_price in price_range) {
    # Calculate new quantity using elasticity
    price_change <- log(new_price / mean(base_data$price, na.rm = TRUE))
    quantity_change <- elasticity * price_change

    new_quantity <- base_data$quantity * exp(quantity_change)
    new_revenue <- new_price * new_quantity

    results <- dplyr::bind_rows(
      results
      , tibble::tibble(
          price = new_price,
          avg_quantity = mean(new_quantity, na.rm = TRUE),
          total_revenue = sum(new_revenue, na.rm = TRUE),
          avg_revenue = mean(new_revenue, na.rm = TRUE)
      )
    )
  }

  return(results)
}

# Simulate revenue across price range
price_range <- seq(30, 80, by = 2)

revenue_sim_naive <- simulate_revenue(ecom_data, price_range, elasticity_naive) %>%
  mutate(method = "Naive OLS")

revenue_sim_iv <- simulate_revenue(ecom_data, price_range, elasticity_iv) %>%
  mutate(method = "2SLS")

# Combine and visualize
revenue_comparison <- dplyr::bind_rows(revenue_sim_naive, revenue_sim_iv)

# Plot revenue curves
ggplot(revenue_comparison, aes(x = price, y = total_revenue, color = method)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = current_stats$avg_price, linetype = "dashed", alpha = 0.7) +
  labs(title = "Revenue Optimization: Impact of Price Elasticity Estimates",
       subtitle = "Dashed line shows current average price",
       x = "Price ($)",
       y = "Total Revenue ($)",
       color = "Estimation Method") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::dollar_format())

# Find optimal prices from simulation
optimal_prices <- revenue_comparison %>%
  dplyr::group_by(method) %>%
  dplyr::slice_max(total_revenue, n = 1) %>%
  dplyr::select(method, optimal_price = price, max_revenue = total_revenue)

print("Optimal Prices from Simulation:")
print(optimal_prices)


# Business Recommendations ----

# Calculate potential revenue impact
current_revenue <- current_stats$total_revenue
optimal_revenue_iv <- optimal_prices %>%
  filter(method == "2SLS") %>%
  pull(max_revenue)

revenue_lift <- (optimal_revenue_iv - current_revenue) / current_revenue * 100

print("BUSINESS RECOMMENDATIONS:")
print("=" %R% 50)
print(paste("Current Average Price: $", round(current_stats$avg_price, 2)))
print(paste("Estimated Price Elasticity: ", round(elasticity_iv, 3)))
print(paste("Recommended Optimal Price: $", round(optimal_prices %>%
                                                    filter(method == "2SLS") %>%
                                                    pull(optimal_price), 2)))
print(paste("Potential Revenue Lift: ", round(revenue_lift, 1), "%"))
print("")
print("KEY INSIGHTS:")
print("- Using IV estimation corrects for endogeneity bias")
print("- Naive OLS may lead to suboptimal pricing decisions")
print("- Shipping cost variation provides valid instrument")
print("- Consider A/B testing to validate elasticity estimates")