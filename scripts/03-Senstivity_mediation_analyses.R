###############################################################################
# 03 Sensititiy and mediation analyses 
# 
# Description: 
#   This script performs mediation analyses for Extended Data Figure S3.
#
# Author: Melissa Cronin
# Created: 2024-06-14
# Last Updated: 2025-09-24
#
# Inputs:
#   - data/SIFI_Index_data.csv
#
# Run Order:
#   This is script 03 in the workflow:
#     01_data_cleaning.R  →  02_analysis_and_visualization.R → 03_Sensitity_mediation_analyses → 04_Bright_spot_analyses
###############################################################################

# Load Required Libraries
###############################################################################
# --- 1. Initialize Workspace -------------------------------------------------
rm(list = ls())  # Clear environment to avoid conflicts

# --- 2. Install and Load Packages -------------------------------------------
required_packages <- c("here",
                       # Data manipulation
                       "dplyr", "tidyr", "forcats", "countrycode","stringr",
                       
                       # Visualization
                       "ggplot2", "viridis", "ggrepel", "scico", "patchwork", "cowplot",
                       
                       # Mapping
                       "sf", "rnaturalearth", "rnaturalearthdata", "cartogram", 
                       "raster",  "maps",
                       
                       # Tables and summaries
                       "table1", "ggpubr",
                       
                       # Utility
                       "scales", "gridExtra", "grid"
)

# Install missing packages
installed_packages <- rownames(installed.packages())
for(pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

# --- 3. Set Global Options ---------------------------------------------------
options(scipen = 999) # turn off scientific notation

# Define color palette
color_range <- c(0, 1)
color_scale <- scale_fill_viridis_c(
  option = "magma",
  direction = -1,
  na.value = "gray",
  limits = color_range
)

###############################################################################

df<- read.csv("data/SIFI_Index_data.csv", sep=",", header=T) %>%
  dplyr::select(
    country_ISO_alpha3,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled,
    adaptive_capacity_inverse_scaled,
    index_mean_scaled,
    crit_1_1,
    crit_1_1_A, crit_1_1_B,
    crit_1_2,
    crit_2_1,
    crit_2_1_A, crit_2_1_B, crit_2_1_C, crit_2_1_D,
    crit_2_2 ,
    crit_2_2_A, crit_2_2_B, crit_2_2_C, 
    crit_3_1 , crit_3_2,
    crit_3_2_A, crit_3_2_B, crit_3_2_C, crit_3_2_D, crit_3_2_E, crit_3_2_F
  ) %>%  drop_na() %>% 
  mutate(
    crit_3_1_inverse = 1 - crit_3_1,
    crit_3_2_inverse = 1 - crit_3_2,
    crit_3_2_A_inverse = 1 - crit_3_2_A,
    crit_3_2_B_inverse = 1 - crit_3_2_B,
    crit_3_2_C_inverse = 1 - crit_3_2_C,
    crit_3_2_D_inverse = 1 - crit_3_2_D,
    crit_3_2_E_inverse = 1 - crit_3_2_E,
    crit_3_2_F_inverse = 1 - crit_3_2_F
  )

# Assuming df is your dataframe and the variables are named as mentioned
pairs <- list(
  E = c("crit_1_1_A", "crit_1_1_B"),
  S = c("crit_2_1", "crit_2_2"),
  AC = c("crit_3_1_inverse", "crit_3_2_inverse")
)

# Function to calculate reliability for each pair
calculate_reliability <- function(df, pairs) {
  reliability_results <- list()
  for (pair_name in names(pairs)) {
    pair_vars <- pairs[[pair_name]]
    if (length(pair_vars) == 2) {
      # Calculate and store the correlation between the two items
      correlation <- cor(df[[pair_vars[1]]], df[[pair_vars[2]]])
      reliability_results[[pair_name]] <- list(
        correlation = correlation
      )
    } else {
      # Calculate and store Cronbach's alpha if more than two items
      reliability_results[[pair_name]] <- list(
        alpha = alpha(df_selected[, pair_vars])$total$raw_alpha
      )
    }
  }
  return(reliability_results)
}

# Calculate reliability for each pair
reliability_results <- calculate_reliability(df, pairs)

# Print the results
print(reliability_results)

#SENSITIVITY ANALYSIS 

# Function to perform sensitivity analysis on pairs
sensitivity_analysis <- function(df, pairs) {
  results <- list()
  for (construct in names(pairs)) {
    indicators <- pairs[[construct]]
    for (i in seq_along(indicators)) {
      # Perform a linear regression of each indicator on the other
      formula <- as.formula(paste(indicators[i], "~", paste(indicators[-i], collapse = "+")))
      model <- lm(formula, data = df)
      results[[paste(construct, indicators[i], sep = "_")]] <- summary(model)$coefficients
    }
  }
  return(results)
}

# Conduct the sensitivity analysis
sensitivity_results <- sensitivity_analysis(df, pairs)

# Check the results
print(sensitivity_results)

#all pairs are significant


#first we want to get the PCs. PC1 is a  weighted sum of the standardized values of the original variables, 
#where the weights are given by the loadings in the first column of the rotation matrix.
# Subset only the six criteria variables
pca_vars <- c("crit_1_1_A", "crit_1_1_B", "crit_1_2", 
              "crit_2_1", "crit_2_2", "crit_3_1", "crit_3_2")  # adjust if it's 6 or 7

df_pca <- df %>% select(all_of(pca_vars))

# Perform Principal Component Analysis
pca_result <- prcomp(df_pca, scale. = TRUE)

# Access the principal components
pcs <- pca_result$x

# Print the results
print(pca_result)

# Access the proportion of variance explained by each PC
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cat("Proportion of Variance Explained:\n", variance_explained, "\n")

# Plot the scree plot to visualize the proportion of variance explained
plot(1:length(variance_explained), variance_explained, type = 'b', 
     main = "Scree Plot",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")

# Biplot (optional)
biplot(pca_result)

#The loadings of each variable on PC1 indicate the contribution of that variable to PC1. 
#Positive loadings mean that increasing values of the variable contribute positively to PC1, 
#while negative loadings mean a negative contribution.

# calculate the composite index
loadings_matrix <- as.matrix(pca_result$rotation) #extracts the rotation matrix from the PCA result. The rotation matrix (also called the loadings matrix) contains the weights assigned to each original variable in the principal components. Each column of this matrix corresponds to a principal component, and each row corresponds to an original variable.
df_standardized <- scale(df_pca) # standardizes the selected variables so that they have a mean of 0 and a standard deviation of 1.
composite_index <- df_standardized %*% loadings_matrix #matrix multiplication between the standardized dataset and loadings matrix- gives  matrix where each row corresponds to an observation (row) in your dataset, and each column corresponds to a principal component 
#values in this^ matrix represent the contributions of each principal component to each observation.

loadings_matrix <- pca_result$rotation

custom_colors <- c("blue", "green", "orange", "purple","red","yellow")
barplot(loadings_matrix, beside = TRUE, col = custom_colors,
        main = "Factor Loadings",
        legend.text = TRUE, args.legend = list(x = "top", bty = "n"))

df_pca$composite_index_PCweights <- rowSums(df_pca * loadings_matrix)

first_pc <- pca_result$x[, 1]
df$first_principal_component <- first_pc

pc_comparison<-df %>% 
  ggplot(aes(x=fct_reorder(country_ISO_alpha3, first_principal_component), y=first_principal_component ,fill=index_mean_scaled))+
  geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="PC1 vulnerability score", fill = "Mean(E, S, AC)")

# Scatter plot of the composite index against the first principal component
plot( df$first_principal_component,df$index_mean_scaled,
      xlab = "Mean(E, S, AC)", ylab = "PC1",
      main = "Score Means vs. PC1 Fit", col = "blue", pch = 16)

# Add regression line
lm_model <- lm( index_mean_scaled~first_principal_component , data = df)
summary(lm_model)
abline(lm_model, col = "red")

# Print the model summary on the plot
summary_text <- paste("Model Fit:\n",
                      "Intercept:", round(coef(lm_model)[1], 3), "\n",
                      "PC1 Coefficient:", round(coef(lm_model)[2], 3), "\n",
                      "R-squared:", round(summary(lm_model)$r.squared, 3))
mtext(summary_text, side = 3, line = -8, adj = 0, col = "red")






# Reorder and rename sensitivity groups
df_cfa <- df %>%
  mutate(
    sensitivity_group = factor(
      sensitivity_group,
      levels = c("Low", "High"),
      labels = c("Low Sensitivity", "High Sensitivity")
    )
  )


# Step 1: Fit the mediator model (Sensitivity as the mediator)
mediator_model <- lm(sensitivity_scaled ~ exposure_scaled, data = df)

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

