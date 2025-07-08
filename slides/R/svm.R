# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Define hyperplane parameters
w <- c(1, -2)  # weight vector
b <- 3         # bias term

# The hyperplane equation is: w1*x1 + w2*x2 + b = 0
# Rearranging: x2 = -(w1*x1 + b)/w2
# So: x2 = -(1*x1 + 3)/(-2) = (x1 + 3)/2

cat("Hyperplane equation: w^T x + b = 0\n")
cat("Substituting values: 1*x1 + (-2)*x2 + 3 = 0\n")
cat("Rearranged: x2 = (x1 + 3)/2\n")
cat("Normal vector w = (", w[1], ",", w[2], ")\n")
cat("||w|| =", sqrt(sum(w^2)), "\n\n")

# Create data for plotting
x1_range <- seq(-8, 4, length.out = 100)
x2_hyperplane <- (x1_range + b) / (-w[2])  # Solve for x2

# Create hyperplane data frame
hyperplane_data <- data.frame(
  x1 = x1_range,
  x2 = x2_hyperplane
)

# Create normal vector data (arrow from origin)
# Scale the vector for better visualization
vector_scale <- 2
normal_vector <- data.frame(
  x1_start = 0,
  x2_start = 0,
  x1_end = 1, # w[1] * vector_scale,
  x2_end = -2 # w[2] * vector_scale
)

# Create some sample points on different sides of the hyperplane
sample_points <- data.frame(
  x1 = c(-2, 0, 2, -4, -6, 0.5, -1),
  x2 = c(0, 2, -1, 3, 0, -2, 4)
)

# Calculate which side each point is on
sample_points$side_value <- w[1] * sample_points$x1 + w[2] * sample_points$x2 + b
sample_points$side <- ifelse(sample_points$side_value > 0, "Positive", "Negative")

# Create the main plot
p1 <- ggplot() +
  # Plot hyperplane
  geom_line(data = hyperplane_data,
            aes(x = x1, y = x2),
            color = "blue",
            size = 1.5,
            alpha = 0.8) +

  # Plot normal vector (arrow)
  geom_segment(data = normal_vector,
               aes(x = x1_start, y = x2_start,
                   xend = x1_end, yend = x2_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "red",
               size = 1.2) +

  # Plot sample points
  geom_point(data = sample_points,
             aes(x = x1, y = x2, color = side, shape = side),
             size = 3,
             alpha = 0.8) +

  # Add origin point
  geom_point(aes(x = 0, y = 0),
             color = "black",
             size = 2) +

  # Add grid and axes
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.3) +

  # Annotations
  annotate("text", x = normal_vector$x1_end + 0.5,
           y = normal_vector$x2_end - 0.5,
           label = "w = (1, -2)",
           color = "red",
           size = 4,
           fontface = "bold") +

  annotate("text", x = 2, y = 1.6,
           label = "x1 - 2x2 + 3 = 0",
           color = "blue",
           size = 4,
           fontface = "bold") +

  annotate("text", x = -6, y = 3,
           label = "Negative side\n(w^T x + b < 0)",
           color = "coral",
           size = 3.5) +

  annotate("text", x = -2.5, y = -2,
           label = "Positive side\n(w^T x + b > 0)",
           color = "lightblue",
           size = 3.5) +

  # Styling
  scale_color_manual(values = c("Negative" = "coral", "Positive" = "lightblue")) +
  scale_shape_manual(values = c("Negative" = 16, "Positive" = 17)) +

  coord_fixed(ratio = 1) +
  xlim(-8, 4) +
  ylim(-4, 4) +

  labs(
    title = "SVM Hyperplane Visualization",
    subtitle = "w = (1, -2), b = 3",
    x = "x1",
    y = "x2",
    color = "Side of Hyperplane",
    shape = "Side of Hyperplane"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    #panel.grid.minor = element_blank()
  )

# Create a second plot showing distance calculations
distance_demo <- data.frame(
  point_name = c("A", "B", "C"),
  x1 = c(-2, 0, 2),
  x2 = c(2, 1, 0)
)

# Calculate distances from points to hyperplane
distance_demo$functional_distance <- w[1] * distance_demo$x1 + w[2] * distance_demo$x2 + b
distance_demo$geometric_distance <- abs(distance_demo$functional_distance) / sqrt(sum(w^2))

p2 <- ggplot() +
  geom_line(data = hyperplane_data,
            aes(x = x1, y = x2),
            color = "blue",
            size = 1.5,
            alpha = 0.8) +

  geom_point(data = distance_demo,
             aes(x = x1, y = x2),
             color = "purple",
             size = 4) +

  geom_text(data = distance_demo,
            aes(x = x1, y = x2, label = point_name),
            vjust = -1,
            size = 4,
            fontface = "bold") +

  coord_fixed(ratio = 1) +
  xlim(-8, 4) +
  ylim(-4, 4) +

  labs(
    title = "Distance from Points to Hyperplane",
    subtitle = "Distance = |w^T x + b| / ||w||",
    x = "x₁",
    y = "x₂"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

# Print the main plot
print(p1)

# Print distance calculations
cat("\nDistance Calculations:\n")
cat("Formula: distance = |w^T x + b| / ||w||\n")
cat("||w|| =", sqrt(sum(w^2)), "\n\n")

for(i in 1:nrow(distance_demo)) {
  point <- distance_demo[i, ]
  cat("Point", point$point_name, "at (", point$x1, ",", point$x2, "):\n")
  cat("  w^T x + b =", point$functional_distance, "\n")
  cat("  Distance =", round(point$geometric_distance, 3), "\n\n")
}

# Create a combined plot
combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))

# Additional mathematical verification
cat("Mathematical Verification:\n")
cat("Hyperplane equation: w^T x + b = 0\n")
cat("Expanded: ", w[1], "*x1 + (", w[2], ")*x2 + ", b, " = 0\n")
cat("Simplified: x1 - 2*x2 + 3 = 0\n")
cat("Solved for x2: x2 = (x1 + 3)/2\n\n")

cat("Normal vector properties:\n")
cat("w = (", w[1], ",", w[2], ")\n")
cat("||w|| = sqrt(", w[1]^2, " + ", w[2]^2, ") =", sqrt(sum(w^2)), "\n")
cat("Unit normal: w/||w|| = (", w[1]/sqrt(sum(w^2)), ",", w[2]/sqrt(sum(w^2)), ")\n")

# Verify that sample points satisfy the classification
cat("\nPoint classification verification:\n")
for(i in 1:nrow(sample_points)) {
  point <- sample_points[i, ]
  cat("Point (", point$x1, ",", point$x2, "): w^T x + b =",
      round(point$side_value, 2), "→", point$side, "\n")
}