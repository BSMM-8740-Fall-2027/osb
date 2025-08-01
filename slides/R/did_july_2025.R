# created July 29, 2025

# 2x2 Difference-in-Differences Example
# Generate synthetic data and create visualization

library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data for 2x2 DiD
# Two groups (treatment and control), two time periods (pre and post)

# Parameters
n_units_per_group <- 100
pre_period <- 0
post_period <- 1
treatment_effect <- 5

# Create base data structure
data <- expand.grid(
  unit_id = 1:(2 * n_units_per_group),
  time = c(pre_period, post_period)
) %>%
  mutate(
    # Assign treatment status (first 100 units are control, next 100 are treatment)
    treated = ifelse(unit_id <= n_units_per_group, 0, 1),

    # Post-treatment indicator
    post = ifelse(time == post_period, 1, 0),

    # Individual fixed effects (some units have higher baseline levels)
    unit_fe = rep(rnorm(2 * n_units_per_group, mean = 10, sd = 2), each = 2),

    # Time trend (common to both groups)
    time_trend = time * 2,

    # Treatment group has slightly different pre-treatment level (but parallel trends)
    group_level = treated * 3,

    # Treatment effect (only for treated units in post period)
    treatment_effect = treated * post * treatment_effect,

    # Random error
    error = rnorm(nrow(.), mean = 0, sd = 1),

    # Generate outcome variable
    outcome = unit_fe + time_trend + group_level + treatment_effect + error
  )

# Calculate group means for visualization
group_means <- data %>%
  group_by(treated, time) %>%
  summarise(
    mean_outcome = mean(outcome),
    se = sd(outcome) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    group_label = ifelse(treated == 1, "Treatment Group", "Control Group")
  )

# Print the 2x2 table
cat("2x2 Difference-in-Differences Table:\n")
cat("=====================================\n\n")

did_table <- group_means %>%
  select(group_label, time, mean_outcome) %>%
  tidyr::pivot_wider(names_from = time, values_from = mean_outcome, names_prefix = "Time_") %>%
  mutate(
    Difference = Time_1 - Time_0
  )

print(did_table)

# Calculate DiD estimate
control_change <- did_table$Difference[did_table$group_label == "Control Group"]
treatment_change <- did_table$Difference[did_table$group_label == "Treatment Group"]
did_estimate <- treatment_change - control_change

cat(sprintf("\nDiD Estimate: %.3f\n", did_estimate))
cat(sprintf("True Treatment Effect: %.3f\n", treatment_effect))

# Create the classic DiD plot
p1 <- ggplot(group_means, aes(x = time, y = mean_outcome,
                              color = group_label, shape = group_label)) +
  geom_point(size = 4) +
  geom_line(aes(group = group_label), size = 1.2) +

  # Add counterfactual line (dashed)
  geom_segment(aes(x = 0, y = group_means$mean_outcome[group_means$treated == 1 & group_means$time == 0],
                   xend = 1, yend = group_means$mean_outcome[group_means$treated == 1 & group_means$time == 0] + control_change),
               linetype = "dashed", color = "red", size = 1) +

  # Customize the plot
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red")) +
  scale_shape_manual(values = c("Control Group" = 16, "Treatment Group" = 17)) +

  labs(
    title = "2x2 Difference-in-Differences",
    subtitle = paste0("DiD Estimate = ", round(did_estimate, 3)),
    x = "Time Period",
    y = "Average Outcome",
    color = "Group",
    shape = "Group"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  ) +

  # Add annotation for counterfactual
  annotate("text", x = 0.7, y = group_means$mean_outcome[group_means$treated == 1 & group_means$time == 0] + control_change + 0.5,
           label = "Counterfactual\n(no treatment)", size = 3.5, color = "red")

print(p1)
did_table |> dplyr::mutate(across(Time_0:Difference, ~round(.x, digits = 2)))
p1 +
annotation_custom(
  gridExtra::tableGrob(
    did_table |> dplyr::mutate(across(Time_0:Difference, ~round(.x, digits = 2))))
    , xmin=0.17, xmax=0.5, ymin=17.5, ymax=20.8
  )

library(ggpmisc)
p1+ annotate(geom = "table", x = 0, y = 20, label = list(did_table |> dplyr::mutate(across(Time_0:Difference, ~round(.x, digits = 2)))),
             vjust = 1, hjust = 0)

# Alternative visualization showing the differences
differences_data <- data.frame(
  Group = c("Control", "Treatment"),
  Before_After_Diff = c(control_change, treatment_change),
  Type = c("Control Change", "Treatment Change")
)

p2 <- ggplot(differences_data, aes(x = Group, y = Before_After_Diff, fill = Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +

  # Add DiD estimate annotation
  annotate("segment", x = 1, xend = 2,
           y = max(differences_data$Before_After_Diff) + 0.5,
           yend = max(differences_data$Before_After_Diff) + 0.5,
           arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +

  annotate("text", x = 1.5, y = max(differences_data$Before_After_Diff) + 1,
           label = paste0("DiD = ", round(did_estimate, 3)), size = 5, fontface = "bold") +

  scale_fill_manual(values = c("Control Change" = "lightblue", "Treatment Change" = "lightcoral")) +

  labs(
    title = "Difference-in-Differences Decomposition",
    subtitle = "DiD = Treatment Change - Control Change",
    x = "Group",
    y = "Before-After Difference",
    fill = "Change Type"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom"
  )

print(p2)

# Run a formal DiD regression
did_regression <- lm(outcome ~ treated + post + treated:post, data = data)
summary(did_regression)

cat("\nRegression Results:\n")
cat("==================\n")
cat("The coefficient on treated:post is the DiD estimate\n")
cat(sprintf("Regression DiD estimate: %.3f\n", coef(did_regression)["treated:post"]))
cat(sprintf("Standard error: %.3f\n", summary(did_regression)$coefficients["treated:post", "Std. Error"]))