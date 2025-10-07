


rm(list=ls())
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(tidyr)
library(viridis)
library(patchwork)


setwd("/Users/melissacronin/Desktop/IHH/country_indicators")
df_with_indicators<-read.csv("df_with_indicators.csv")%>%
  mutate(Country = ifelse(Country == "Brazil (inland & marine, separately)", "Brazil", Country))
#check for BRIGHT SPOTS 

library(ggpattern)

low_exposure_plot_data <- df_with_indicators %>%
  #filter(FAO.Sub.Region %in% c("West Africa", "Eastern Asia", "Southern Asia", "South-Eastern Asia", "Middle Africa")) %>% 
 # filter(FAO.Sub.Region %in% c("West Africa", "South-Eastern Asia", "Middle Africa")) %>% 
   group_by(FAO.Sub.Region) %>% # Group by subregion
  mutate(
    mean_exposure = median(exposure_scaled, na.rm = TRUE), # Mean for exposure
    mean_sensitivity = median(sensitivity_scaled, na.rm = TRUE), # Mean for sensitivity
    mean_adaptive_capacity = median(adaptive_capacity_scaled, na.rm = TRUE) # Mean for adaptive capacity
  ) %>%
  filter(
    exposure_scaled <= mean_exposure, # Below or equal to the mean for exposure
    sensitivity_scaled >= mean_sensitivity, # Above or equal to the mean for sensitivity
    adaptive_capacity_scaled <= mean_adaptive_capacity # Above or equal to the mean for adaptive capacity
  ) %>%

 # mutate(lower_quartile = quantile(exposure_scaled, 0.20, na.rm = TRUE)) %>% # Calculate 25th percentile within each subregion
  #filter(exposure_scaled <= 0.1) %>% # Filter for values in the lowest quartile
  ungroup() %>% # Remove grouping
    dplyr::select(
    Country, FAO.Sub.Region,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled
  ) %>%
  rename(
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_scaled
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component", values_to = "Score"
  ) %>% 
  mutate(Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")),
         BarType = "Country",
     #    FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("Southern Asia","Eastern Asia","West Africa", "South-Eastern Asia",  "Middle Africa")) # Reorder facets
        FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("West Africa", "South-Eastern Asia",  "Middle Africa")) # Reorder facets
       
        ) # Label for country-level data


# Calculate regional means using ALL countries in each region
regional_means <- df_with_indicators %>%
   #filter(FAO.Sub.Region %in% c("West Africa", "South-Eastern Asia",  "Middle Africa")) %>%
 # filter(FAO.Sub.Region %in% c("Southern Asia","Eastern Asia","West Africa", "South-Eastern Asia",  "Middle Africa")) %>% 
   dplyr::select(
    FAO.Sub.Region,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled
  ) %>%
  rename(
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_scaled
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component", values_to = "Score"
  ) %>%
  group_by(FAO.Sub.Region, Component) %>%
  summarise(
    Country = "Regional Mean",
    Score = mean(Score, na.rm = TRUE),
    BarType = "Regional Mean", # Label for regional mean
    .groups = "drop"
  )%>%
  mutate(
    Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")), # Ensure same factor levels
    FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("West Africa", "South-Eastern Asia",  "Middle Africa")) # Reorder facets
   #   FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("Southern Asia","Eastern Asia","West Africa", "South-Eastern Asia",  "Middle Africa")) # Reorder facets
  )

# Combine country-level data with regional means
combined_plot_data_sub <- bind_rows(low_exposure_plot_data, regional_means)

custom_colors <- c("Exposure" = "#D55E00", # Vibrant red-orange
                   "Sensitivity" = "royalblue", # Muted gray
                   "Adaptive Capacity" = "lightblue") # Light blue

combined_plot_data_sub <- combined_plot_data_sub %>%
  group_by(FAO.Sub.Region) %>%
  mutate(Country = factor(
    Country,
    levels = c("Regional Mean", unique(Country[Country != "Regional Mean"]))
  )) %>%
  ungroup()


# Create the plot
subregion_low_exp <- ggplot(combined_plot_data_sub, aes(x =Country,  y = Score, fill = Component, pattern = BarType)) +
  facet_wrap(~FAO.Sub.Region, scales = "free_x", nrow=1) + # Facet by FAO subregion
  scale_fill_manual(values = custom_colors)+
  scale_pattern_manual(values = c("Country" = "none", "Regional Mean" = "stripe")) + # Set patterns
  labs(
    x = "Country",
    y = "Component Scores",
    fill = ""

  ) +
  theme_classic() +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    pattern_fill = "gray",
    pattern_density = 0.001, # Adjust density of the pattern
    pattern_spacing = 0.05,
    pattern_angle = 45
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    text = element_text(size = 15),
    legend.position = "top"
  )




subregion_low_exp
ggsave("Revised_bright_spots.jpeg", dpi=300, width=10, height=7)


df_with_indicators %>% 
 mutate(FAO.Region = fct_reorder(FAO.Region, exposure_scaled, .fun = median, .desc = TRUE)) %>% # Reorder by median
  ggplot() +
  geom_boxplot(aes(x = FAO.Region, y = exposure_scaled)) +
  coord_flip() +
  labs(
    x = "FAO Sub-Region",
    y = "Exposure (Scaled)",
    title = "Boxplot of Exposure by FAO Sub-Region"
  ) +
  theme_minimal()



#PERCENT CHANGE


library(tidyverse)

# Filter for selected FAO subregions
selected_subregions <- c("West Africa", "South-Eastern Asia", "Middle Africa")

df_filtered <- df_with_indicators %>%
  #filter(FAO.Sub.Region %in% selected_subregions) %>%
 # group_by(FAO.Sub.Region) %>%
  group_by(FAO.Region) %>%
  mutate(
    median_exposure = mean(exposure_scaled, na.rm = TRUE),
    median_sensitivity = mean(sensitivity_scaled, na.rm = TRUE),
    median_adaptive_capacity = mean(adaptive_capacity_scaled, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Filter only countries that meet the criteria
  filter(
    exposure_scaled < median_exposure,         # Lower Exposure than median
    sensitivity_scaled > median_sensitivity,   # Higher Sensitivity than median
    adaptive_capacity_scaled < median_adaptive_capacity  # Lower Adaptive Capacity than median
  ) %>%
  mutate(
    Exposure_Delta = ((exposure_scaled - median_exposure) / median_exposure) * 100,
    Sensitivity_Delta = ((sensitivity_scaled - median_sensitivity) / median_sensitivity) * 100,
    Adaptive_Capacity_Delta = ((adaptive_capacity_scaled - median_adaptive_capacity) / median_adaptive_capacity) * 100
  ) %>%

  select(Country, FAO.Region, Exposure_Delta, Sensitivity_Delta, Adaptive_Capacity_Delta) %>%
  pivot_longer(
    cols = c(Exposure_Delta, Sensitivity_Delta, Adaptive_Capacity_Delta),
    names_to = "Component",
    values_to = "Percent_Change"
  ) %>%
  mutate(Component = factor(Component, levels = c("Exposure_Delta", "Sensitivity_Delta", "Adaptive_Capacity_Delta"))) 


  
  
# Rename components for clarity
df_filtered$Component <- recode(df_filtered$Component,
                                "Exposure_Delta" = "Exposure",
                                "Sensitivity_Delta" = "SSF reliance",
                                "Adaptive_Capacity_Delta" = "Adaptive Capacity"
)

# Plot the percent change for each country
ggplot(df_filtered, aes(x = Country, y = Percent_Change, fill = Component)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  facet_wrap(~FAO.Region, scales = "free_x", nrow=1) +
  scale_fill_manual(values = c("Exposure" = "#D55E00", "SSF reliance" = "royalblue", "Adaptive Capacity" = "lightblue")) +
  labs(
    x = "Country",
    y = "% change from regional mean", fill=""
     ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") + # Baseline for percent change
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        text = element_text(size = 18),
        legend.position = "top")

# Save the plot
ggsave("Filtered_Countries_Percent_Change.jpeg", dpi = 300, width = 8, height = 6)




















#REGION LEVEL

df_with_indicators %>%
  group_by(FAO.Region) %>%
  summarise(
    lower_quartile_exposure = quantile(exposure_scaled, 0.3, na.rm = TRUE),
    mean_sensitivity = mean(sensitivity_scaled, na.rm = TRUE),
    mean_adaptive_capacity = mean(adaptive_capacity_scaled, na.rm = TRUE)
  )

low_exposure_plot_data <- df_with_indicators %>%
  group_by(FAO.Region) %>% # Group by subregion
  mutate(
    lower_quartile_exposure = quantile(exposure_scaled, 0.25, na.rm = TRUE), # 25th percentile for exposure
    upper_quartile_sensitivity = quantile(sensitivity_scaled, 0.5, na.rm = TRUE), # 75th percentile for sensitivity
    upper_quartile_adaptive_capacity = quantile(adaptive_capacity_scaled, 0.5, na.rm = TRUE) # 75th percentile for adaptive capacity
  ) %>%
  # filter(
  #   exposure_scaled <= lower_quartile_exposure, # Lowest quartile for exposure
  #   sensitivity_scaled >= upper_quartile_sensitivity, # Highest quartile for sensitivity
  #   adaptive_capacity_inverse_scaled >= upper_quartile_adaptive_capacity # Highest quartile for adaptive capacity
  # ) %>%
  mutate(
    mean_exposure = median(exposure_scaled, na.rm = TRUE), # Mean for exposure
    mean_sensitivity = median(sensitivity_scaled, na.rm = TRUE), # Mean for sensitivity
    mean_adaptive_capacity = median(adaptive_capacity_scaled, na.rm = TRUE) # Mean for adaptive capacity
  ) %>%
  filter(
    exposure_scaled <= mean_exposure, # Below or equal to the mean for exposure
    sensitivity_scaled >= mean_sensitivity, # Above or equal to the mean for sensitivity
    #adaptive_capacity_scaled <= mean_adaptive_capacity # Above or equal to the mean for adaptive capacity
    adaptive_capacity_scaled <= upper_quartile_adaptive_capacity
    ) %>%
  
   ungroup() %>% # Remove grouping
  dplyr::select(
    Country, FAO.Region,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled
  ) %>%
  rename(
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_scaled
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component", values_to = "Score"
  ) %>% 
  mutate(Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")),
         BarType = "Country",
        # FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("West Africa", "Middle Africa","Southern Asia", "Eastern Asia", "South-Eastern Asia")) # Reorder facets
  ) # Label for country-level data


# Calculate regional means using ALL countries in each region
regional_means <- df_with_indicators %>%
  dplyr::select(
    FAO.Region,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled
  ) %>%
  rename(
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_scaled
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component", values_to = "Score"
  ) %>%
  group_by(FAO.Region, Component) %>%
  summarise(
    Country = "Regional Median",
    Score = mean(Score, na.rm = TRUE),
    BarType = "Regional Median", # Label for regional mean
    .groups = "drop"
  )%>%
  mutate(
    Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")), # Ensure same factor levels
 #   FAO.Sub.Region = factor(FAO.Sub.Region, levels = c("West Africa", "Middle Africa", "Eastern Asia", "Southern Asia","South-Eastern Asia")) # Reorder facets
  )

# Combine country-level data with regional means
combined_plot_data <- bind_rows(low_exposure_plot_data, regional_means)

custom_colors <- c("Exposure" = "#D55E00", # Vibrant red-orange
                   "Sensitivity" = "royalblue", # Muted gray
                   "Adaptive Capacity" = "lightblue") # Light blue

combined_plot_data <- combined_plot_data %>%
  group_by(FAO.Region) %>%
  mutate(Country = factor(
    Country,
    levels = c("Regional Median", unique(Country[Country != "Regional Median"]))
  )) %>%
  ungroup()

# Create the plot
region_low_exp <- ggplot(combined_plot_data, aes(x = Country, y = Score, fill = Component, pattern = BarType)) +
  facet_wrap(~FAO.Region, scales = "free_x", nrow=1) + # Facet by FAO subregion
  scale_fill_manual(values = custom_colors)+
  scale_pattern_manual(values = c("Country" = "none", "Regional Median" = "stripe", guide = "none")) + # Set patterns
  labs(
    x = "Country",
    y = "SIFI Component Scores",
    fill = "Vulnerability Component"
  ) +
  theme_classic() +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    pattern_fill = "gray",
    pattern_density = 0.001, # Adjust density of the pattern
    pattern_spacing = 0.05,
    pattern_angle = 45
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    text = element_text(size = 16),
    legend.position = "top"
  )


region_low_exp









require(patchwork)
(low_exp_countries + subregion_low_exp) + plot_layout(guides = "collect")& theme(legend.position = 'top')


ggsave("bright_spot_plot.tiff", dpi=300, width=10, height=7)


#bright spots - NON GEOGRAPHIC 

# Calculate global means for all components
global_means <- df_with_indicators %>%
  summarise(
    global_sensitivity_mean = mean(sensitivity_scaled, na.rm = TRUE),
    global_adaptive_capacity_mean = mean(adaptive_capacity_inverse_scaled, na.rm = TRUE)
  )

global_sensitivity_mean <- global_means$global_sensitivity_mean
global_adaptive_capacity_mean <- global_means$global_adaptive_capacity_mean

global_means <- df_with_indicators %>%
  summarise(
    Exposure = mean(exposure_scaled, na.rm = TRUE),
    Sensitivity = mean(sensitivity_scaled, na.rm = TRUE),
    `Adaptive Capacity` = mean(adaptive_capacity_inverse_scaled, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component",
    values_to = "Score"
  ) %>%
  mutate(
    Country = "Global Mean",
    BarType = "Global Mean",
    Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity"))
  )

# quartiles <- df_with_indicators %>%
#   summarise(
#     exposure_25th = quantile(exposure_scaled, 0.25, na.rm = TRUE),
#     sensitivity_75th = quantile(sensitivity_scaled, 0.50, na.rm = TRUE),
#     adaptive_capacity_75th = quantile(adaptive_capacity_inverse_scaled, 0.50, na.rm = TRUE)
#   )
# 
# # Extract thresholds
# exposure_25th <- quartiles$exposure_25th
# sensitivity_75th <- quartiles$sensitivity_75th
# adaptive_capacity_75th <- quartiles$adaptive_capacity_75th


# Subset low-exposure countries with additional filters
selected_countries <- df_with_indicators %>%
  filter(exposure_scaled <= 0.08) %>% # Filter for low exposure
  filter(sensitivity_scaled > global_sensitivity_mean) %>%  # Sensitivity higher than global mean
  filter(  adaptive_capacity_inverse_scaled > global_adaptive_capacity_mean # Adaptive Capacity higher than global mean
  ) %>%
  
  # filter(
  #   exposure_scaled <= exposure_25th, # Lowest quartile for exposure
  #   sensitivity_scaled >= sensitivity_75th, # Highest quartile for sensitivity
  #   adaptive_capacity_inverse_scaled >= adaptive_capacity_75th # Highest quartile for adaptive capacity
  # ) %>%
  dplyr::select(
    Country, FAO.Region,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_inverse_scaled
  ) %>%
  rename(
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_inverse_scaled
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component",
    values_to = "Score"
  ) %>%
  mutate(
    Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")),
    BarType = "Country"
     )


regional_means <- df_with_indicators %>%
  group_by(FAO.Region) %>%
  summarise(
    Exposure = mean(exposure_scaled, na.rm = TRUE),
    Sensitivity = mean(sensitivity_scaled, na.rm = TRUE),
    `Adaptive Capacity` = mean(adaptive_capacity_inverse_scaled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Exposure, Sensitivity, `Adaptive Capacity`),
    names_to = "Component",
    values_to = "Score"
  ) %>%
  mutate(
    Country = "Regional Mean",
    BarType = "Regional Mean",
    Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity"))
  )

# Combine country-level data with global means
combined_plot_data <- bind_rows(selected_countries, regional_means)

# Reorder the Country factor to place "Regional Mean" first within each FAO.Sub.Region
combined_plot_data <- combined_plot_data %>%
  group_by(FAO.Region) %>%
  mutate(Country = factor(
    Country,
    levels = c("Regional Mean", unique(Country[Country != "Regional Mean"]))
  )) %>%
  ungroup()
# Custom colors for the components
custom_colors <- c("Exposure" = "#D55E00", # Vibrant red-orange
                   "Sensitivity" = "royalblue", # Blue
                   "Adaptive Capacity" = "lightblue") # Light blue

grouped_comparison_plot <- ggplot(combined_plot_data, aes(x = reorder(Country, -Score), y = Score, fill = Component, pattern = BarType)) +
  facet_wrap(~FAO.Region, scales = "free_x", nrow = 1) + # Facet by FAO subregion
  scale_fill_manual(values = custom_colors) +
  scale_pattern_manual(values = c("Country" = "none", "Regional Mean" = "stripe")) + # Set patterns
  labs(
    x = "Country",
    y = "Component Scores",
    fill = "Vulnerability Component"
  ) +
  theme_classic() +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    pattern_fill = "gray",
    pattern_density = 0.001, # Adjust density of the pattern
    pattern_spacing = 0.05,
    pattern_angle = 45
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    text = element_text(size = 14),
    legend.position = "top"
  )

# Display the plot
grouped_comparison_plot





#WEST AFRICA ONLY 

# Define thresholds for "low" and "high" based on scaled values (e.g., below/above median)
exposure_threshold <- median(df_with_indicators$exposure_scaled, na.rm = TRUE)
sensitivity_threshold <- median(df_with_indicators$sensitivity_scaled, na.rm = TRUE)
adaptive_capacity_threshold <- median(df_with_indicators$adaptive_capacity_scaled, na.rm = TRUE)
colnames(df_with_indicators)

# Identify countries with low exposure and high sensitivity & adaptive capacity
low_exposure_countries <- df_with_indicators %>%
  filter(
    exposure_scaled < exposure_threshold,
    sensitivity_scaled > sensitivity_threshold,
    adaptive_capacity_inverse_scaled > adaptive_capacity_threshold
  ) %>%
  dplyr::select(
    Country = Country,
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_inverse_scaled
  ) %>%
  arrange(Exposure) %>% 
  pivot_longer(cols = c(Exposure, Sensitivity, `Adaptive Capacity`), 
               names_to = "Component", values_to = "Score") %>% 
  mutate(Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")))


# Create the bar plot
low_exp_countries<- ggplot(low_exposure_countries, aes(x = reorder(Country, -Score), y = Score, fill = Component)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option="magma",direction = -1) +
  labs(
    x = "Country",
    y = "Component Scores",
    fill = "Vulnerability Component"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        text = element_text(size = 16),
        legend.position = "top")

low_exp_countries

ggsave("low_exposure_vulnerability.tiff", dpi=300, width=7, height=7)

#case studies - WEST AFRICA

# Identify countries with low exposure and high sensitivity & adaptive capacity
low_exposure_countries <- df_with_indicators %>%
  filter(iso_code=="GMB"|
           iso_code=="GNB"|
           iso_code=="SEN"|
           iso_code=="GIN"|
           iso_code=="LBR"|
           iso_code== "SLE") %>%  
  dplyr::select(
    Country = Country,
    Exposure = exposure_scaled,
    Sensitivity = sensitivity_scaled,
    `Adaptive Capacity` = adaptive_capacity_inverse_scaled
  ) %>%
  arrange(Exposure) %>% 
  pivot_longer(cols = c(Exposure, Sensitivity, `Adaptive Capacity`), 
               names_to = "Component", values_to = "Score") %>% 
  mutate(Component = factor(Component, levels = c("Exposure", "Sensitivity", "Adaptive Capacity")))


# Create the bar plot
low_exp_countries_WAFRICA<- ggplot(low_exposure_countries, aes(x = reorder(Country, -Score), y = Score, fill = Component)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values= c("#3B0F70", "#BB3754", "#D9A100"),
                    name = "Vulnerability Component"
  ) +
  
  labs(
    x = "Country",
    y = "Component Scores",
    fill = "Vulnerability Component"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        text = element_text(size = 16),
        legend.position = "top")

low_exp_countries_WAFRICA

