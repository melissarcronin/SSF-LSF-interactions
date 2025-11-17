
###############################################################################
# 04 Bright spots analyses
# 
# Description: 
#   This script uses the SIFI Index data to evaluate bright spot countries.
#
# Author: Melissa Cronin
# Created: 2024-06-14
# Last Updated: 2025-09-24
#
# Inputs:
#   - data/SIFI_Index_data.csv
#
# Run Order:
#   This is script 04 in the workflow:
#     01_data_cleaning.R  →  02_analysis_and_visualization.R → 03_Sensitity_mediation_analyses → 04_Bright_spot_analyses
###############################################################################

# Load Required Libraries
###############################################################################
# --- 1. Initialize Workspace -------------------------------------------------
rm(list = ls())  # Clear environment to avoid conflicts

# --- 2. Install and Load Packages -------------------------------------------
required_packages <- c("here",
                       # Data manipulation
                       "dplyr", "tidyr", "forcats", "scales", "tidyverse","countrycode",
      
                       # Visualization
                       "ggplot2", "viridis", "ggrepel", "scico", "patchwork"
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

colnames(df)
df<- read.csv("data/df_with_SIFI_score.csv", sep=",", header=T) %>%
  dplyr::select(
    Alpha.3.code,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled,
    adaptive_capacity_inverse_scaled,
    index_mean_scaled,
    SIFI_Index ) %>% 
  mutate( Country = countrycode(Alpha.3.code,
                                     origin = "iso3c",
                                     destination = "country.name"),
    FAO.Region = countrycode(Alpha.3.code,
                             origin = "iso3c",
                             destination = "region"),
    FAO.Subregion = countrycode(Alpha.3.code,
                                origin = "iso3c",
                                destination = "region23"),
    FAO.Continent = countrycode(Alpha.3.code,
                                origin = "iso3c",
                                destination = "continent")
  )

paa_data<- read.csv("data/preferential_access_areas_Basurto_2024.csv", sep=",", header=T) %>% 
  rename(Alpha.3.code=Alpha_code_3) %>% as.data.frame()

# Get vector of countries that have PAAs
paa_countries <- unique(paa_data$Alpha.3.code)







bright_spot_deltas <- df %>%
  group_by(FAO.Subregion) %>%
  mutate(
    mean_exposure          = mean(exposure_scaled, na.rm = TRUE),
    mean_sensitivity       = mean(sensitivity_scaled, na.rm = TRUE),
    mean_adaptive_capacity = mean(adaptive_capacity_scaled, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Exposure_Delta          = ((exposure_scaled          - mean_exposure) / mean_exposure) * 100,
    Sensitivity_Delta       = ((sensitivity_scaled       - mean_sensitivity) /mean_sensitivity) * 100,
    Adaptive_Capacity_Delta = ((adaptive_capacity_scaled - mean_adaptive_capacity) / mean_adaptive_capacity) * 100
  ) %>%
  # keep ONLY countries that match bright-spot criteria: higher Exposure, higher Sensitivity, lower Adaptive Capacity than regional median
  filter( Exposure_Delta > 0,
         Sensitivity_Delta > 0,
         Adaptive_Capacity_Delta < 0 ) %>%
  select(Country, Alpha.3.code, FAO.Subregion,
         Exposure_Delta, Sensitivity_Delta, Adaptive_Capacity_Delta) %>%
  
  pivot_longer(
    cols = c(Exposure_Delta, Sensitivity_Delta, Adaptive_Capacity_Delta),
    names_to = "Component",
    values_to = "Percent_Change"
  ) %>%
  mutate(
    Component = dplyr::case_when(
      Component == "Exposure_Delta"          ~ "Exposure",
      Component == "Sensitivity_Delta"       ~ "Sensitivity",
      Component == "Adaptive_Capacity_Delta" ~ "Adaptive Capacity",
      TRUE ~ Component
    ),
    Component = factor(Component,
                       levels = c("Exposure", "Sensitivity", "Adaptive Capacity"))
  ) %>% 
  mutate(
    has_PAA = if_else(Alpha.3.code %in% paa_countries, TRUE, FALSE),
    label = if_else(has_PAA,
                    paste0(Country, " ★"),   # add star to denote it has a PAA 
                    Country)
  )

custom_colors <- c(
  "Exposure"          = "#D55E00",
  "Sensitivity"       = "royalblue",
  "Adaptive Capacity" = "lightblue"
)
bright_spot_deltas %>% 
ggplot(  aes(x = Country,
                         y = Percent_Change,
                         fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(~ FAO.Subregion, scales = "free_x", space = "free_x",  labeller = labeller(FAO.Subregion = label_wrap_gen(width = 12))) +
  
  scale_fill_manual(values = custom_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_text(
    data = subset(bright_spot_deltas, has_PAA == TRUE & Component == "Exposure"),
    aes(x = Country,
        y = Percent_Change + 5),   # 5 units above the bar
    label = "*",
    size = 6,
    fontface = "bold",
    inherit.aes = FALSE
  )+
  labs(
    x = "Country (ISO3)",
    y = "% change from regional median",
    fill = ""
  ) +
  theme_classic() +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    text = element_text(size = 14),
    legend.position = "top"
  )



#WEST AFRICA ONLY for an illustrative example. 

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

