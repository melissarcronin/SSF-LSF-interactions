###############################################################################
###############################################################################
# 02 Construct SIFI Index using confirmatory factor analysis
# 
# Description: 
#   This script uses cleaned data to construct and validate the SIFI Index of vulnerability. 
#
# Author: Melissa Cronin
# Created: 2024-06-14
# Last Updated: 2025-12-23
#
# Inputs:
#   - data/SIFI_Index_data.csv
#
# Run Order:
#   This is script 02 in the workflow:
#     01_data_cleaning.R  →  02_analysis_and_visualization.R → 03_Bright_spot_analysis
###############################################################################

# Load Required Libraries
###############################################################################
# --- 1. Initialize Workspace -------------------------------------------------
rm(list = ls())  # Clear environment to avoid conflicts

# --- 2. Install and Load Packages -------------------------------------------
required_packages <- c(
  # Project & utilities
  "here", "scales", "gridExtra", "grid", "stringr",
 
  "dplyr", "tidyr", "forcats", "countrycode", "psych", "likert",
  "broom", "purrr","ggpattern",
  "ggplot2", "viridis", "ggrepel", "scico", "patchwork", "cowplot", "ggpubr", "plotly",
  
  # Mapping
  "sf", "rnaturalearth", "rnaturalearthdata", "cartogram", "raster", "maps",

  
  # Tables and summaries
  "table1",
  
  # SEM and stats
  "lavaan", "semPlot", "lavaanPlot"
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
df<- read.csv(here("data", "processed", "SIFI_Index_data.csv"), sep=",", header=T) %>%
  dplyr::select(
    Alpha.3.code,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_scaled,
    adaptive_capacity_inverse_scaled,
    index_mean_scaled,
    crit_1_1,
    crit_1_1_A,
    crit_1_1_B,
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


#####################################################################################
### CFA to construct SIFI Index 
#####################################################################################

## CFA MODEL #######################################################################

# Define a vector of the fit measures you want to extract
desired_measures <- c("cfi", "tli", "rmsea", "srmr", "chisq", "pvalue", "AIC", "BIC")

colnames(df)
# CFA model to idnetify important drivers of vulnerability 
model_V <- '
    vulnerability =~    crit_1_2+  crit_1_1_B+crit_1_1_A 
  vulnerability =~  crit_2_1 +crit_2_2 
  vulnerability =~ crit_3_1+ crit_3_2
  
  # Add covariances based on modification indices (determined below)
crit_1_2 ~~ crit_1_1_B
crit_2_1 ~~ crit_1_1_A

 '
png(here("outputs", "figures", "vulnerability_cfa_sem_plot.png"),  width = 4800, height = 3600, res = 600)
cfa_model_V <- cfa(model_V, data=df, estimator="MLMV", bootstrap = 1000)

semPaths(cfa_model_V, what = "est", 
         whatLabels = "std",
         #curvePivot = TRUE,
         layout = "tree",  style = "ram",
          nodeLabels = c( "SSF catch",
                          "Non-broadcasting LSF density",
                           "AIS broadcasting LSF density",
                             "SSF economic value",
                            "SSF nutrition",
                            "development","governance",
                         "Vulnerability"),
         sizeMan = 13, 
         sizeLat = 20,
         sizeLat2= 5, 
         sizeMan2=10,
         residuals = FALSE,
         sizeInt2=1, 
         fade = FALSE,
         #edge.color = ifelse(lavInspect(cfa_model_V, "std")$lambda > 0, "royalblue", "red3"), # Change colors based on sign
         nCharNodes = 0.8, # To reduce the space between nodes, set to 0 or adjust as necessary
         edge.label.cex = 0.8,
         edge.color = ifelse(
           lavInspect(cfa_model_V, "std")$lambda > 0,
           "royalblue",           # positive association
           "red3"                 # negative association
         ) )


dev.off()
summary(cfa_model_V, fit.measures=TRUE,  standardized = TRUE)
lavInspect(cfa_model_V, "theta")
# Check the fit of the model
fit_measures_V <- fitMeasures(cfa_model_V, fit.measures = desired_measures)
fit_measures_V
vartable(cfa_model_V)

mod_indices <- modificationindices(cfa_model_V)
# Sort by largest MI
mod_indices <- mod_indices[order(mod_indices$mi, decreasing = TRUE), ]
# View top 10 suggested modifications
head(mod_indices, 10)

lavInspect(cfa_model_V, "cov.lv")

# Plot the CFA results




# Calculate the factor scores -these are the SIFI scores!
factor_scores <- lavPredict(cfa_model_V, type = "lv")

# Add factor scores to your dataframe
df$cfa_vulnerability_score <- factor_scores

cfa_comparison<-df %>% 
  ggplot(aes(x=fct_reorder(country_ISO_alpha3, -cfa_vulnerability_score), y=-cfa_vulnerability_score ,fill= index_mean_scaled))+
  geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="CFA vulnerability score", fill = "Mean(E, S, AC)")


df<- df %>% 
  mutate(cfa_mean_vulnerability= rowMeans( cfa_vulnerability_score)) %>% 
  mutate(SIFI_Index= scales::rescale(cfa_mean_vulnerability),
         Continent = countrycode(Alpha.3.code, origin = "iso3c", destination = "continent"),
         country = countrycode(Alpha.3.code, origin = "iso3c", destination = "country.name")
  )

world_shp <- sf::st_as_sf(maps::map("world", plot = F, fill = TRUE))
world_shp_iso <- world_shp %>%
  mutate(Alpha.3.code = countrycode(sourcevar = ID, origin = "country.name", destination = "iso3c"))

df_spatial <- merge(world_shp_iso, df,by="Alpha.3.code" )
colnames(df_spatial)
SIFI_map<-ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill =SIFI_Index), color = "lightgrey")+
  color_scale+
  labs(fill = "SIFI Index") +
  theme_void() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))
SIFI_map

ggsave(
  here("outputs", "figures", "SIFI_map.tiff"),
  device = "tiff",
  dpi=300, width=8, height=6)


exposure_map<- ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill =exposure_scaled), color = "lightgrey")+
  color_scale+
  labs(fill = "Exposure") +
  theme_void() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

sensitivity_map <- ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill =sensitivity_scaled), color = "lightgrey")+
  color_scale+
  labs(fill = "Sensitivity") +
  theme_void() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

adaptive_capacity_map <- ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill =adaptive_capacity_scaled), color = "lightgrey")+
  color_scale+
  labs(fill = "Adaptive capacity") +
  theme_void() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

exposure_map / sensitivity_map / adaptive_capacity_map
  

ggsave(
  here("outputs", "figures", "SIFI_component_map.tiff"),
  device = "tiff",
  dpi=300, width=6, height=9)


# Scatter plot of the composite index against the simpler methods of calculating the SIFI Index
plot(df$index_mean_scaled, df$SIFI_Index,
     xlab = "Mean(E, S, AC)", ylab = "CFA vulnerability score",
     main = "Score Means vs. CFA Fit", col = "blue", pch = 16)

# Add regression line
lm_model <- lm(SIFI_Index ~ index_mean_scaled, data = df)
summary(lm_model)
abline(lm_model, col = "red")

# Print the model summary on the plot
summary_text <- paste("Model Fit:\n",
                      "Intercept:", round(coef(lm_model)[1], 3), "\n",
                      "CFA Coefficient:", round(coef(lm_model)[2], 3), "\n",
                      "R-squared:", round(summary(lm_model)$r.squared, 3))
mtext(summary_text, side = 3, line = -8, adj = 0, col = "red")


# Plot the CFA results
factor_loadings <- lavInspect(cfa_model_V, "std.lv")
# Extract factor names and loadings separately
factor_names <- colnames(factor_loadings[[1]])
loadings <- as.vector(factor_loadings[[1]])

# Create a data frame
loadings_df <- as.data.frame(as.table(factor_loadings$lambda))
colnames(loadings_df) <- c("variable", "factor", "loading")

ggplot(loadings_df, aes(x = factor, y = loading, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Factor Loadings", x = "Factor", y = "Loading") +
  theme_minimal() +
  theme(legend.position = "top")

#this plot shows factor loadings, which represent the strength and direction of the relationships between the latent variables (factors) 
#and the observed variables (indicators or items) in your confirmatory factor analysis (CFA) model. 
#In a CFA model, each latent variable is associated with a set of observed variables, and the factor loadings indicate 
#how much each observed variable contributes to the corresponding latent factor.



# SEM Model Specification
#examine relationships between constructs 
colnames(df)
sem_model <- '
# Measurement Model
# baseline indicator, fixed to 1 for identification

E =~ exposure_scaled
#E =~ crit_1_1_A+ crit_1_1_B + crit_1_2
#E =~ crit_1_1+ crit_1_2

S =~ crit_2_1 + crit_2_2
#S =~ crit_2_1_A + crit_2_1_B+ crit_2_1_C +  crit_2_1_D+ crit_2_2_A + crit_2_2_B + crit_2_2_C

AC =~ crit_3_1 + crit_3_2

# -----------------------------
# Structural Model
# -----------------------------
# Regression paths capture hypothesized causal effects
# S is regressed on E: captures the effect of exposure on sensitivity


E  ~~ AC
S  ~~ AC
S~~E

 #crit_2_2 ~~ crit_3_1
 #crit_2_2 ~~ crit_3_2
 #crit_2_1 ~~ crit_3_1
# crit_1_1_B~~crit_3_2
# crit_1_1_B~~crit_2_2
# crit_1_1_A~~crit_3_2
# crit_1_1_B~~crit_1_1_A

'

# Fitting the SEM model
sem_result <- sem(sem_model, data = df, estimator="MLR", std.lv=TRUE)

# Summarizing the results with fit measures
summary(sem_result, fit.measures = TRUE)
fit_measures_sem <- fitMeasures(sem_result, fit.measures = desired_measures)
fit_measures_sem


# Plotting the SEM results
png(here("outputs", "figures", "components_sem_plot.png"),  width = 4800, height = 3600, res = 600)

semPaths(sem_result,  style = "lisrel", curvePivot = TRUE, layout = "circle",
         what = "std",         # Show raw/unstandardized estimates
         whatLabels = "std" ,
         posCol = c("royalblue", "royalblue"),
         negCol = c("red3", "red3")      
)

dev.off()
lavInspect(sem_result, "cov.lv")

mod_indices <- modificationindices(sem_result)

# Sort by largest MI
mod_indices <- mod_indices[order(mod_indices$mi, decreasing = TRUE), ]

# View top 10 suggested modifications
head(mod_indices, 10)



# ---- Sensitivity vs Exposure ----

# Fit the linear model
lm_fit_A <- lm(sensitivity_scaled ~ exposure_scaled, data = df)
# Extract coefficients and statistics
beta_A <- round(coef(lm_fit_A)[2], 2)
p_val_A <- summary(lm_fit_A)$coefficients[2, 4]
# Format p-value
p_label_A <- ifelse(p_val_A < 0.001, "p < 0.001",
                  ifelse(p_val_A < 0.01, "p < 0.01",
                         paste0("p = ", round(p_val_A, 3))))
label_text_A <- paste0("β = ", beta_A, ", ", p_label_A)

A<- df %>%
  ggplot(aes(x = exposure_scaled, y = sensitivity_scaled)) +
  geom_point(color = "grey", size = 3, alpha = 0.6) +   # larger, semi-transparent grey points
  geom_smooth(method = "lm", color = "#1976D2", fill = "lightblue", se = TRUE) +  # jeans blue line
  annotate("text", x = max(df$exposure_scaled)*0.3, y = max(df$sensitivity_scaled)*0.9,
           label = label_text_A, hjust = 0, size = 5) +  # place text in top-right

  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Exposure", y="Sensitivity")+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )
A
# ---- AC vs Exposure ----
lm_fit_AC_E <- lm(adaptive_capacity_scaled ~ exposure_scaled, data = df)

beta_AC_E <- round(coef(lm_fit_AC_E)[2], 2)
p_val_AC_E <- summary(lm_fit_AC_E)$coefficients[2, 4]

p_label_AC_E <- ifelse(p_val_AC_E < 0.001, "p < 0.001",
                       ifelse(p_val_AC_E < 0.01, "p < 0.01",
                              paste0("p = ", round(p_val_AC_E, 3))))

label_text_AC_E <- paste0("β = ", beta_AC_E, ", ", p_label_AC_E)

B<- ggplot(df, aes(x = exposure_scaled, y = adaptive_capacity_scaled)) +
  geom_point(color = "grey50", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", se = TRUE) +
  annotate("text", x = max(df$exposure_scaled)*0.3, y = max(df$adaptive_capacity_scaled)*0.9,
           label = label_text_AC_E, hjust = 0, size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  labs(x = "Exposure", y = "Adaptive Capacity")

# ---- AC vs Sensitivity ----
lm_fit_AC_S <- lm( adaptive_capacity_scaled~sensitivity_scaled, data = df)

beta_AC_S <- round(coef(lm_fit_AC_S)[2], 2)
p_val_AC_S <- summary(lm_fit_AC_S)$coefficients[2, 4]

p_label_AC_S <- ifelse(p_val_AC_S < 0.001, "p < 0.001",
                       ifelse(p_val_AC_S < 0.01, "p < 0.01",
                              paste0("p = ", round(p_val_AC_S, 3))))

label_text_AC_S <- paste0("β = ", beta_AC_S, ", ", p_label_AC_S)

C<-ggplot(df, aes(x =  sensitivity_scaled, y=adaptive_capacity_scaled)) +
  geom_point(color = "grey50", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", se = TRUE) +
  annotate("text", x = max(df$sensitivity_scaled)*0.3, y = max(df$adaptive_capacity_scaled)*0.9,
           label = label_text_AC_S, hjust = 0, size = 5) +
  theme_classic() +
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  labs(x = "Adaptive capacity", y = "Sensitivity")

A+C+B

ggsave(
  here("outputs", "figures", "component_lms.tiff"),
  device = "tiff",
  dpi=300, width=14, height=6)



model_AC <- lm(adaptive_capacity_scaled ~ exposure_scaled + sensitivity_scaled, data = df)
summary(model_AC)



#add grouped bars by CONTINENTS
selected_columns <- c("country", "SIFI_Index", "Continent")
# Subset the data frame
df_subset <- df[selected_columns]
df_subset$country <- factor(df_subset$country, levels = unique(df_subset$country[order(df_subset$Continent)]))
region_order <- c( "Europe/Asia", "Europe","Oceania","Americas","Asia", "Africa" )

# Order the levels of FAO.Region
df_subset$Continent <- factor(df_subset$Continent, levels = region_order)
df_subset$country<- factor(df_subset$country, levels = unique(df_subset$country[order(df_subset$Continent, df_subset$SIFI_Index)]))
df_subset <- df_subset %>%
  mutate(SIFI_centered = SIFI_Index - 0.5)

P<-ggplot(df_subset, aes(x = country, y =SIFI_centered,fill = SIFI_Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "SIFI Index of Vulnerability") +
  theme_classic() +
  color_scale+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +  
  coord_flip() +  # Flip coordinates
  scale_y_continuous(
    limits = c(-0.5, 0.5),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
    labels = c("0", "0.25", "0.5", "0.75", "1.0")   # artificial SIFI scale
  ) +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 6)
  )+
  scale_x_discrete(expand = c(0.03, 0.03)) 
P


ggsave(
  here("outputs", "figures", "SIFI_countries_grouped.tiff"),
  device = "tiff",
  dpi=300, height=10, width=5)



##########################################################################################################################
#check if PAA is related to any of the variables using PAA data from Basurto et al. 2024
##########################################################################################################################


paa_data<- read.csv(here("data", "raw", "preferential_access_areas_Basurto_2024.csv"), sep=",", header=T) %>% 
  rename(Alpha.3.code=Alpha_code_3) %>% as.data.frame()

# Get vector of countries that have PAAs
paa_countries <- unique(paa_data$Alpha.3.code)

# Add a binary / factor flag to df
df <- df %>%
  mutate(
    has_PAA = if_else(Alpha.3.code %in% paa_countries, "Has PAA", "No PAA")
  )

# List of metrics to test
metrics <- c("exposure_scaled", "sensitivity_scaled", "adaptive_capacity_scaled")

# Function to run t-test and return summary stats
run_tests <- function(var) {
  t <- t.test(df[[var]] ~ df$has_PAA)
  tibble(
    metric = var,
    mean_Has_PAA = mean(df[[var]][df$has_PAA == "Has PAA"], na.rm = TRUE),
    mean_No_PAA  = mean(df[[var]][df$has_PAA == "No PAA"], na.rm = TRUE),
    sd_Has_PAA   = sd(df[[var]][df$has_PAA == "Has PAA"], na.rm = TRUE),
    sd_No_PAA    = sd(df[[var]][df$has_PAA == "No PAA"], na.rm = TRUE),
    n_Has_PAA    = sum(df$has_PAA == "Has PAA", na.rm = TRUE),
    n_No_PAA     = sum(df$has_PAA == "No PAA", na.rm = TRUE),
    t_stat       = t$statistic,
    df           = t$parameter,
    p_value      = t$p.value
  )
}

# Run tests for each variable
paa_summary_table <- map_df(metrics, run_tests) %>% 
  mutate(
    metric = dplyr::case_when(
      metric == "exposure_scaled" ~ "Exposure",
      metric == "sensitivity_scaled" ~ "Sensitivity",
      metric == "adaptive_capacity_scaled" ~ "Adaptive Capacity",
      TRUE ~ metric
    )
  )
paa_summary_table

write.csv( paa_summary_table,
  here("outputs", "data_tables", "paa_summary_table.csv"))

# Reshape data from wide to long for plotting
df_long <- df %>%
  dplyr::select(has_PAA, exposure_scaled, sensitivity_scaled, adaptive_capacity_scaled) %>%
  pivot_longer(cols = c(exposure_scaled, sensitivity_scaled, adaptive_capacity_scaled),
               names_to = "metric",
               values_to = "value") %>% 
  mutate(
    PAA = factor(
      ifelse(has_PAA == "TRUE" | has_PAA == "Has PAA", "Has PAA", "No PAA"),
      levels = c("Has PAA", "No PAA")
    )
  )

# labels 
df_long <- df_long %>%
  mutate(
    metric = dplyr::case_when(
      metric == "exposure_scaled" ~ "Exposure",
      metric == "sensitivity_scaled" ~ "Sensitivity",
      metric == "adaptive_capacity_scaled" ~ "Adaptive Capacity",
      TRUE ~ metric
    )
  )


component_colors <- c(
  "Exposure"          = "#D55E00",
  "Sensitivity"       = "royalblue",
  "Adaptive Capacity" = "lightblue"
)


ggplot(df_long, aes(x = metric, y = value, fill=has_PAA)) +
  geom_jitter(
    aes(color = metric, group = has_PAA),
    alpha = 0.25,
    width = 0.15,
    size = 1,
    show.legend = FALSE
  ) +
  geom_boxplot(
    aes(fill = metric,group = interaction(metric, has_PAA)),
    alpha = 0.8,
    outlier.shape = NA,
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = component_colors) +
  scale_color_manual(values = component_colors) +
  theme_classic() +
  labs(
    x = "Component of vulnerability",
    y = "Scaled value",
    fill = "Component"
  ) +
  theme( text = element_text(size = 14), 
         axis.text.x = element_text(size = 13, angle = 20, hjust = 1) )+ 
  geom_text(data = paa_summary_table, aes(x = metric, y = max(df_long$value) + 0.05, # adjust if needed 
            label = ifelse(p_value < .001, "***", ifelse(p_value < .01, "**", ifelse(p_value < .05, "*", "ns")))), inherit.aes = FALSE, size = 6)



