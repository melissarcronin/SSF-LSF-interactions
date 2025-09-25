

rm(list=ls())
library(scales)
library(dplyr)
library(lavaan)
library(dplyr)
library(likert)
library(psych)
library(maps)
library(cartogram)
library(scales)
library(ggplot2)
library(tidyverse)
library(semPlot)
library(lavaanPlot)
#library(tidyr)
library(viridis)
library(ggrepel)
library(grid)
library(gridExtra)
library(plotly)
library(table1) 
library(ggpubr)
library(ggplot2)
library(scico)
library(patchwork)
library(rgdal)
library(forcats)
library(sf)
library(raster)
library(rnaturalearthdata)
library(rnaturalearth)
library(rgeos)
library(countrycode)
options(scipen=999)


setwd("/Users/melissacronin/Desktop/factor_analysis")
df<- read.csv("vulnerability_index.results.csv", sep=",", header=T)

df_selected_full <- df %>%
  dplyr::select(
    country_ISO_alpha3,
    country,
    exposure_scaled,
    sensitivity_scaled,
    adaptive_capacity_inverse_scaled,
    index_mean_scaled,
    crit_1_1,
    crit_1_1_A, crit_1_1_B,
    crit_1_2,
    crit_2_1,
    crit_2_1_A, crit_2_1_B, crit_2_1_C, crit_2_1_D,
    crit_2_2 ,
    crit_2_2_A, crit_2_2_B, crit_2_2_C, crit_2_2_D,
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



# Select the relevant columns
df_selected <- df_selected_full %>%
  dplyr::select(
     crit_1_1_A,
     crit_1_1_B,
    crit_1_2,
    crit_2_1,
    crit_2_2,
    crit_3_1_inverse,
    crit_3_2_inverse
  
   #  exposure_scaled,
    #sensitivity_scaled,
    #adaptive_capacity_inverse_scaled
  ) %>% 
  drop_na()




#RELIABILITT ANALYSIS: measure of internal consistency of the indicators 

# Assuming df is your dataframe and the variables are named as mentioned
pairs <- list(
  E = c("crit_1_1_A", "crit_1_1_B"),
  S = c("crit_2_1", "crit_2_2"),
  AC = c("crit_3_1_inverse", "crit_3_2_inverse")
)

# Function to calculate reliability for each pair
calculate_reliability <- function(df_selected, pairs) {
  reliability_results <- list()
  for (pair_name in names(pairs)) {
    pair_vars <- pairs[[pair_name]]
    if (length(pair_vars) == 2) {
      # Calculate and store the correlation between the two items
      correlation <- cor(df_selected[[pair_vars[1]]], df_selected[[pair_vars[2]]])
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
reliability_results <- calculate_reliability(df_selected, pairs)

# Print the results
print(reliability_results)

#SENSITIVITY ANALYSIS 

# Function to perform sensitivity analysis on pairs
sensitivity_analysis <- function(df_selected, pairs) {
  results <- list()
  for (construct in names(pairs)) {
    indicators <- pairs[[construct]]
    for (i in seq_along(indicators)) {
      # Perform a linear regression of each indicator on the other
      formula <- as.formula(paste(indicators[i], "~", paste(indicators[-i], collapse = "+")))
      model <- lm(formula, data = df_selected)
      results[[paste(construct, indicators[i], sep = "_")]] <- summary(model)$coefficients
    }
  }
  return(results)
}

# Conduct the sensitivity analysis
sensitivity_results <- sensitivity_analysis(df_selected, pairs)

# Check the results
print(sensitivity_results)

#all pairs are significantly 


#first we want to get the PCs. PC1 is a  weighted sum of the standardized values of the original variables, 
#where the weights are given by the loadings in the first column of the rotation matrix.

# Perform Principal Component Analysis
pca_result <- prcomp(df_selected, scale. = TRUE)

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
df_standardized <- scale(df_selected) # standardizes the selected variables so that they have a mean of 0 and a standard deviation of 1.
composite_index <- df_standardized %*% loadings_matrix #matrix multiplication between the standardized dataset and loadings matrix- gives  matrix where each row corresponds to an observation (row) in your dataset, and each column corresponds to a principal component 
#values in this^ matrix represent the contributions of each principal component to each observation.

loadings_matrix <- pca_result$rotation

custom_colors <- c("blue", "green", "orange", "purple","red","yellow")
barplot(loadings_matrix, beside = TRUE, col = custom_colors,
        main = "Factor Loadings",
        legend.text = TRUE, args.legend = list(x = "top", bty = "n"))

df_selected_full$composite_index_PCweights <- rowSums(df_selected * loadings_matrix)

first_pc <- pca_result$x[, 1]
df_selected_full$first_principal_component <- first_pc
#df_selected_full$first_principal_component <- -df_selected_full$first_principal_component

pc_comparison<-df_selected_full %>% 
ggplot(aes(x=fct_reorder(country, first_principal_component), y=first_principal_component ,fill=index_mean_scaled))+
 geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="PC1 vulnerability score", fill = "Mean(E, S, AC)")

# Scatter plot of the composite index against the first principal component
plot(df_selected_full$index_mean_scaled, df_selected_full$first_principal_component,
     xlab = "Mean(E, S, AC)", ylab = "PC1",
     main = "Score Means vs. PC1 Fit", col = "blue", pch = 16)

# Add regression line
lm_model <- lm(first_principal_component ~ index_mean_scaled, data = df_selected_full)
summary(lm_model)
abline(lm_model, col = "red")

# Print the model summary on the plot
summary_text <- paste("Model Fit:\n",
                      "Intercept:", round(coef(lm_model)[1], 3), "\n",
                      "PC1 Coefficient:", round(coef(lm_model)[2], 3), "\n",
                      "R-squared:", round(summary(lm_model)$r.squared, 3))
mtext(summary_text, side = 3, line = -8, adj = 0, col = "red")


# Create a 3D scatter plot with plotly
plot_ly(
  x = df_selected$exposure_scaled,
  y = df_selected$sensitivity_scaled,
  z = df_selected$adaptive_capacity_inverse_scaled,
  mode = "markers",
  marker = list(color = "blue", size = 6),
  type = "scatter3d",
  name = "Original Components"
) %>%
  add_trace(
    x = first_pc,
    y = first_pc,
    z = first_pc,
    mode = "markers",
    marker = list(color = "red", size = 6),
    type = "scatter3d",
    name = "PC1"
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "Exposure"),
      yaxis = list(title = "Sensitivity"),
      zaxis = list(title = "Adaptive Capacity Inverse")
    ),
    title = "PC1 vs. Original Components"
  )



cfa_model <- '
vulnerability =~  crit_1_2 +crit_1_1 + crit_2_1 + crit_2_2 + crit_3_1_inverse + crit_3_2_inverse

#E =~ crit_1_1 *crit_1_2

#E =~ crit_1_1_A+crit_1_1_B
#E=~crit_1_2

#S =~   crit_2_1 + crit_2_2

#S =~   crit_2_1_A+ crit_2_1_B+ crit_2_1_C+crit_2_1_D #replace w RAW data. 
#S =~ crit_2_2_A+ crit_2_2_B+ crit_2_2_C+ crit_2_2_D

#AC =~ crit_3_1_inverse + crit_3_2_inverse

#AC=~crit_3_2_A_inverse+ crit_3_2_B_inverse+ crit_3_2_C_inverse+ crit_3_2_D_inverse+ crit_3_2_E_inverse+ crit_3_2_F_inverse

  
# Factor Covariances

crit_1_1 ~~ crit_1_2 
crit_2_1 ~~ crit_2_2 
crit_3_1_inverse ~~ crit_3_2_inverse

#S ~~ AC
#E ~~ S
#E ~~ AC

'

# Run the CFA
cfa_model <- cfa(cfa_model, data=df_selected_full, estimator="MLR")

# Check the fit of the model
summary(cfa_model, fit.measures=TRUE)

# Plot the CFA results
semPaths(cfa_model, what = "std", style = "lisrel",
         curvePivot = TRUE,layout = "spring")


lavInspect(cfa_model, "cov.lv")
mod_indices <- modificationindices(cfa_model)
print(mod_indices, sort = TRUE)

# Calculate the factor scores
factor_scores <- lavPredict(cfa_model, type = "lv")

# Add factor scores to your dataframe
df_selected_full$cfa_vulnerability_score <- factor_scores

cfa_comparison<-df_selected_full %>% 
  ggplot(aes(x=fct_reorder(country, -cfa_vulnerability_score), y=-cfa_vulnerability_score ,fill= index_mean_scaled))+
  geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="CFA vulnerability score", fill = "Mean(E, S, AC)")


# Scatter plot of the composite index against the first principal component
plot(df_selected_full$index_mean_scaled, -df_selected_full$cfa_vulnerability_score,
     xlab = "Mean(E, S, AC)", ylab = "CFA vulnerability score",
     main = "Score Means vs. CFA Fit", col = "blue", pch = 16)

# Add regression line
lm_model <- lm(-cfa_vulnerability_score ~ index_mean_scaled, data = df_selected_full)
summary(lm_model)
abline(lm_model, col = "red")

# Print the model summary on the plot
summary_text <- paste("Model Fit:\n",
                      "Intercept:", round(coef(lm_model)[1], 3), "\n",
                      "CFA Coefficient:", round(coef(lm_model)[2], 3), "\n",
                      "R-squared:", round(summary(lm_model)$r.squared, 3))
mtext(summary_text, side = 3, line = -8, adj = 0, col = "red")


####CFA ######
#dont use chi sq values  
#CFI - values .95 and above are good, .90 are acceptabel fit. If lower=lack of fit.
#TLI (or non nrom fit)
#Root mean sq error (RMSEA) of 0.05 or below are considered close fit. non-sig p value indicates close fit
#SRMR of 0.05 or below = close fit model

# CFA: Latent Factors - unobserved constructs or factors that you are hypothesizing to exist
# Observed Variables - variables that you have measured or observed directly. They are indicators of the latent factors.



# Plot the CFA results
factor_loadings <- lavInspect(cfa_model, "std.lv")
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







cor(df_selected_full$crit_1_1, df_selected_full$crit_1_2)
#SEM

# SEM Model Specification
sem_model <- '
# Measurement Model

E =~ crit_1_1 + crit_1_2
#E =~ crit_1_1_A+crit_1_1_B

#S =~ crit_2_1 + crit_2_2
S =~   crit_2_1_A+ crit_2_1_B+ crit_2_1_C+crit_2_1_D
S =~ crit_2_2_A+ crit_2_2_B+ crit_2_2_C+ crit_2_2_D

AC =~ crit_3_1_inverse + crit_3_2_inverse
#AC =~ crit_3_1_inverse 
#AC=~crit_3_2_A_inverse+ crit_3_2_B_inverse+ crit_3_2_C_inverse+ crit_3_2_D_inverse+ crit_3_2_E_inverse+ crit_3_2_F_inverse

# Structural Model

S ~ E  # S is regressed on E
AC ~ S # AC is regressed on S
AC ~ E # AC is regressed on E
'

# Fitting the SEM model
sem_result <- sem(sem_model, data = df_selected_full)

# Summarizing the results with fit measures
summary(sem_result, fit.measures = TRUE)

# Plotting the SEM results
semPaths(sem_result, what = "std", style = "lisrel", curvePivot = TRUE, layout = "tree")

lavInspect(sem_result, "cov.lv")
mod_indices <- modificationindices(sem_result)
print(mod_indices, sort = TRUE)

# Calculate the factor scores
factor_scores <- lavPredict(sem_result, type = "lv")

# Add factor scores to your dataframe
df_selected_full$vulnerability_score_sem <- factor_scores

df_selected_full<-df_selected_full %>% 
  mutate(sem_mean_vulnerability= rowMeans( vulnerability_score_sem)) %>% 
  mutate(sem_vulnerability=scales::rescale(sem_mean_vulnerability))

colnames(df_selected_full)
sem_comparison<-df_selected_full %>% 
  ggplot(aes(x=fct_reorder(country, sem_mean_vulnerability), y=sem_mean_vulnerability ,fill=index_mean_scaled))+
  geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="SEM vulnerability score", fill = "Mean(E, S, AC)")

pc_comparison+ cfa_comparison + sem_comparison
ggsave("index_methods.tiff", dpi=300 , width=20, height=13)

# Scatter plot of the composite index against the first principal component
plot(df_selected_full$index_mean_scaled, df_selected_full$sem_vulnerability,
     xlab = "Mean(E, S, AC)", ylab = "CFA vulnerability score",
     main = "Score Means vs. SEM Fit", col = "blue", pch = 16)

# Add regression line
lm_model <- lm(sem_vulnerability ~ index_mean_scaled, data = df_selected_full)
summary(lm_model)
abline(lm_model, col = "red")

# Print the model summary on the plot
summary_text <- paste("Model Fit:\n",
                      "Intercept:", round(coef(lm_model)[1], 3), "\n",
                      "SEM Coefficient:", round(coef(lm_model)[2], 3), "\n",
                      "R-squared:", round(summary(lm_model)$r.squared, 3))
mtext(summary_text, side = 3, line = -8, adj = 0, col = "red")

ggplot(df_selected_full) +
  geom_histogram(aes(x =sem_vulnerability), na.rm = TRUE, bins = 30) +
  #  color_scale +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Vulnerability")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

setwd("/Users/melissacronin/Desktop/IHH/country_indicators/")
country_indicators<-read.csv("recent_country_indicators.csv",sep=",", header=T) %>% 
  rename(Alpha.3.code = ISO.Alpha3 )
colnames(df_selected_full)
df_new_indicators<- df_selected_full %>% 
  rename(Alpha.3.code= country_ISO_alpha3) %>% 
  left_join(country_indicators, by = "Alpha.3.code") %>% 
  mutate(cfa_mean_vulnerability= rowMeans( cfa_vulnerability_score)) %>% 
  mutate(pc1_scaled=scales::rescale(first_principal_component),
         cfa_scaled= scales::rescale(mean(cfa_mean_vulnerability)),
         sem_scaled=scales::rescale(sem_vulnerability))

world_shp <- sf::st_as_sf(maps::map("world", plot = F, fill = TRUE))


world_shp_iso <- world_shp %>%
  mutate(Alpha.3.code = countrycode(sourcevar = ID, origin = "country.name", destination = "iso3c"))

df_spatial <- merge(world_shp_iso, df_new_indicators,by="Alpha.3.code" )

ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill = first_principal_component), color = "lightgrey")+
  #color_scale+
  scale_fill_viridis_c(option="inferno", direction=-1)+
  labs(fill = "Vulnerability Index") +
  theme_classic() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))+
  geom_hline(yintercept = 0,  color = "darkgrey", alpha = 0.5,  linewidth = 0.2) +  # Equator
  geom_hline(yintercept = 23.5, linetype = "dashed",color = "darkgrey", alpha = 0.8, linewidth=0.2) +  # Tropic of Cancer
  geom_hline(yintercept = -23.5, linetype = "dashed", color = "darkgrey", alpha = 0.8, linewidth = 0.2)  # Tropic of Capricorn





#add grouped bars by CONTINENTS
selected_columns <- c("ID", "pc1_scaled", "FAO.Region")
# Subset the data frame
df_subset <- df_spatial[selected_columns]
df_subset$Country <- factor(df_subset$ID, levels = unique(df_subset$ID[order(df_subset$FAO.Region)]))
region_order <- c( "Europe/Asia", "Europe","Oceania","Americas","Asia", "Africa" )
# Order the levels of FAO.Region
df_subset$FAO.Region <- factor(df_subset$FAO.Region, levels = region_order)
df_subset$ID<- factor(df_subset$ID, levels = unique(df_subset$ID[order(df_subset$FAO.Region, df_subset$pc1_scaled)]))


P<-ggplot(df_subset, aes(x = ID, y =pc1_scaled,fill = pc1_scaled)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Vulnerability") +
  theme_classic() +
  color_scale+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +  
  coord_flip() +  # Flip coordinates
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 6)
  )+
  scale_x_discrete(expand = c(0.03, 0.03)) 
P
ggsave("countries_grouped.tiff", dpi=300, height=10, width=5)

#check for effect of region
# Perform anova
region_anova_result <- aov(pc1_scaled ~ FAO.Region, data = df_spatial)
p_value_region <- summary(region_anova_result)[[1]]$`Pr(>F)`[1]

# Print the ANOVA result
summary(region_anova_result)
region_order <- c(  "Africa" , "Asia", "Oceania","Americas","Europe/Asia","Europe")
# Order the levels of FAO.Region
df_spatial$FAO.Region <- factor(df_spatial$FAO.Region, levels = region_order)

region_compared<-
  ggplot(df_spatial, aes(y = pc1_scaled, x = FAO.Region)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="A",y = "Vulnerability", x = "Region") +
  theme_classic()+
  annotate("text", x = 3, y = max(df_spatial$pc1_scaled) * 0.95, 
           label = ifelse(region_anova_result$p.value > 0.01, "p > 0.01", ifelse(region_anova_result$p.value < 0.001, "p < 0.001", paste("p =", signif(region_anova_result$p.value, digits = 3)))),
           hjust = 1, size = 4)+
  theme(text = element_text(size = 14))




df_spatial$LeastDeveloped_2018 <- factor(ifelse(is.na(df_spatial$LeastDeveloped_2018) | df_spatial$LeastDeveloped_2018 == "no", "no", "yes"), levels = c("yes", "no"))
t_test_LDC <- t.test(pc1_scaled ~ LeastDeveloped_2018, data = df_spatial)

LDC_compared<-ggplot(df_spatial, aes(y = pc1_scaled,x = LeastDeveloped_2018)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="B", x = "Development level", y = "Vulnerability") +
  theme_classic() +
  theme(text = element_text(size = 14))+
  scale_x_discrete(labels = c("yes" = "LDC", "no" = "Non-LDC"))+
  annotate("text", x = 1.8, y = max(df_spatial$pc1_scaled) * 0.95, 
           label = ifelse(t_test_LDC$p.value > 0.01, "p > 0.01", ifelse(t_test_LDC$p.value < 0.001, "p < 0.001", paste("p =", signif(t_test_tropical$p.value, digits = 3)))),
           hjust = 1, size = 4)


#check for effect of tropcial 

df_spatial <- df_spatial %>%
  mutate(Tropical = ifelse(Proportion_TropicsZone > 0.75, "Tropical", "Not Tropical"))
df_spatial$Tropical <- factor(df_spatial$Tropical, levels = c("Tropical", "Not Tropical"))

df_spatial <- df_spatial[complete.cases(df_spatial$Tropical), ]

#t test
t_test_tropical <- t.test(pc1_scaled ~ Tropical, data = df_spatial)

tropical_compared<- ggplot(df_spatial, aes(y = pc1_scaled,x = Tropical)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="C", y = "Vulnerability", x = "Country type") +
  theme_classic() +
  annotate("text", x = 1.8, y = max(df_spatial$pc1_scaled) * 0.95, 
           label = ifelse(t_test_tropical$p.value > 0.01, "p > 0.01", 
                          ifelse(t_test_tropical$p.value < 0.001, "p < 0.001",
                                 paste("p =", signif(t_test_tropical$p.value, digits = 3)))),
           hjust = 1, size = 4)+
  theme(text = element_text(size = 14))

boxplots<-region_compared+(LDC_compared+tropical_compared )
boxplots

ggsave("boxplots.tiff", dpi=300, height=7, width=12)
