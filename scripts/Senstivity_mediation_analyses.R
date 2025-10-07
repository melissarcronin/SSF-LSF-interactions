library(ggplot2)
library(dplyr)
library(patchwork)  # For combining plots

# Reorder and rename sensitivity groups
df_cfa <- df_cfa %>%
  mutate(
    sensitivity_group = factor(
      sensitivity_group,
      levels = c("Low", "High"),
      labels = c("Low Sensitivity", "High Sensitivity")
    )
  )


# Step 1: Fit the mediator model (Sensitivity as the mediator)
mediator_model <- lm(sensitivity_scaled ~ exposure_scaled, data = df_cfa)

# Step 2: Fit the outcome model (Adaptive Capacity as the outcome)
outcome_model <- lm(adaptive_capacity_scaled ~ exposure_scaled + sensitivity_scaled, data = df_cfa)

# Step 3: Perform mediation analysis
mediation_result <- mediate(
  model.m = mediator_model, 
  model.y = outcome_model, 
  treat = "exposure_scaled", 
  mediator = "sensitivity_scaled", 
  boot = TRUE, 
  sims = 1000 # Number of bootstrap simulations
)

# Step 4: Summarize and interpret the mediation results
summary(mediation_result)

# Optional: Visualize the mediation effect
plot(mediation_result)

# Main scatter plot with trends
scatter_plot <- df_cfa %>%
  ggplot(aes(x = exposure_scaled, y = adaptive_capacity_scaled, color = sensitivity_group)) +
  geom_point(size = 4, alpha = 0.3, stroke = 0) +
  geom_smooth(method = 'lm', aes(fill = sensitivity_group), alpha = 0.2) +
  geom_text(aes(label = iso_code), size = 3, hjust = 0.5, vjust = -0.5, alpha = 0.8) +
  theme_classic() +
  facet_wrap(~sensitivity_group, nrow = 1) +
  scale_color_manual(values = c("High Sensitivity" = "blue", "Low Sensitivity" = "orange")) +
  scale_fill_manual(values = c("High Sensitivity" = "blue", "Low Sensitivity" = "orange")) +
  labs(
    x = "Exposure",
    y = "Adaptive Capacity",
    #title = "Effect of Exposure on Adaptive Capacity by Sensitivity Level",
    color = "Sensitivity Group",
    fill = "Sensitivity Group"
  ) +
  theme(text = element_text(size = 14))

# Density plot for exposure by sensitivity group
density_plot <- df_cfa %>%
  ggplot(aes(x = exposure_scaled, fill = sensitivity_group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("High Sensitivity" = "blue", "Low Sensitivity" = "orange")) +
  theme_classic() +
  labs(
    x = "Exposure",
    y = "Density",
   # title = "Distribution of Exposure by Sensitivity Group",
    fill = "Sensitivity Group" ) +
  theme(text = element_text(size = 14))





# Combine scatter and density plots
combined_plot <- scatter_plot / density_plot

# Display the combined plot
print(combined_plot)


heatmap_data <- df_cfa %>%
  group_by(
    adaptive_capacity_group = cut(adaptive_capacity_scaled, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")),
    sensitivity_group = cut(sensitivity_scaled, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  ) %>%
  summarize(mean_exposure = mean(exposure_scaled, na.rm = TRUE), .groups = "drop")

# Heatmap plot
ggplot(heatmap_data, aes(x = sensitivity_group, y = adaptive_capacity_group, fill = mean_exposure)) +
  geom_tile(color = "white") +  # Add gridlines for clarity
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Exposure") +  # Color gradient for exposure
  labs(
    x = "Sensitivity",
    y = "Adaptive Capacity",
    title = "Heatmap of Exposure by Adaptive Capacity and Sensitivity"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

