

#Now, load fishing data, which should be scaled by country's square km within 25km. each variable shouold be scaled.

# setwd("/Users/melissacronin/Desktop/global_GFW_data/Fishing25km")
# fishing_hours<-read.csv( "SUMFishing25kmBuff.csv", sep=",", header = T) %>% 
#   dplyr::select(ISO_TER1, fishing_hours) %>% 
#   group_by(ISO_TER1) %>% 
#   summarise(fishing_hours=sum(fishing_hours, na.rm = TRUE)) # %>%   filter(ISO_TER1!="CHN")
# 
# presence_hours<-read.csv( "SUMFishing25kmBuff.csv", sep=",", header = T) %>% 
#   dplyr::select(ISO_TER1,hours, fishing_hours)  %>% 
#   group_by(ISO_TER1) %>% 
#   summarise(presence_hours=sum(hours, na.rm = TRUE)) # %>%  filter(ISO_TER1!="CHN")
# 
# vessels_present<-read.csv( "VesselsPresentCount25km.csv", sep=",", header = T) %>% 
#   dplyr::select(ISO_TER1, mmsi)%>% 
#   group_by(ISO_TER1) %>% 
#    summarise(vessels_present_count=sum( unique(mmsi), na.rm = TRUE))# %>%  filter(ISO_TER1!="CHN")
# 
# vessels_fishing<-read.csv( "VesselsFishingCount25km.csv", sep=",", header = T) %>% 
#   dplyr::select(ISO_TER1,  mmsi) %>% 
#   group_by(ISO_TER1) %>% 
#   summarise(vessels_fishing_count=sum(unique(mmsi), na.rm = TRUE))# %>%  filter(ISO_TER1!="CHN")
# 
# # Left join fishing_hours and vessels_present
# merged_df <-fishing_hours %>% 
#   left_join( presence_hours, by = c("ISO_TER1"), relationship = "many-to-many") %>% 
#   left_join(vessels_present,  by = c("ISO_TER1")) %>% 
#   left_join(vessels_fishing,  by = c("ISO_TER1")) %>% 
#   rename(iso_code=ISO_TER1)
# 
# sum(merged_df$vessels_fishing_count)

# # Scale the variables by square kilometers of the 100 km band
# final_df <- merged_df%>%
#   filter(!(
#     is.na(presence_hours) & #filter countries with no data at all. retain countries with <4 variables
#       is.na(fishing_hours) &
#       is.na(vessels_fishing_count) &
#       is.na(vessels_present_count)))
# 
# merged_data <- left_join(final_df,coastal_buffer_final, by = c("iso_code")) 
# 
# 
# crit_1_1_A_data<- merged_data %>% 
#   mutate_all(~ifelse(is.na(.), 0, .)) %>% 
#   ungroup() %>% 
#   mutate(   hours  =  (presence_hours + fishing_hours )/ sum_area, 
#                  vessel_density =(vessels_fishing_count+vessels_present_count) /sum_area   ) %>%  
#   mutate(across(everything(), ~ifelse(. == "Inf", 0, .))) %>% 
#   mutate(crit_1_1_A_raw= hours+vessel_density ) %>% 
#   mutate(crit_1_1_A_log= log(crit_1_1_A_raw +1)) %>% 
#   mutate(crit_1_1_A  =scales::rescale(crit_1_1_A_log) ) %>% 
#   rename(Alpha.3.code=iso_code) %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(-geometry) %>% 
#   filter(crit_1_1_A!="-Inf")
# 
# #write.csv(crit_1_1_data, "crit_1_1_data.csv")
# 
# crit_1_1_A_data %>% 
# ggplot()+
#   geom_histogram(aes(x=crit_1_1_A))
# 
# shapiro.test(crit_1_1_A_data$crit_1_1_A)
# 
# 
### 2_2_C WHO undernourishment indicator (share of population with insufficient caloric intake) ######
#Description: Prevalence of undernourishments is the percentage of the population whose habitual food consumption is insufficient to provide the dietary energy levels that are required to maintain a normal active and healthy life. Data showing as 2.5 may signify a prevalence of undernourishment below 2.5%.
#so higher undernourishment score = more need
# setwd("/Users/melissacronin/Desktop/IHH/IHH_country_criteria_prioritization/undernourishment_data")
# undernourishment<- read.csv("undernourishment_data.csv", sep=",", header=T)# %>%   filter(X2013!="..") %>%  filter(country!="China")
# 
# # Convert selected columns to numeric
# year_columns <- colnames(undernourishment)[grepl("^X201[3-7]$", colnames(undernourishment))]
# undernourishment[year_columns] <- lapply(undernourishment[year_columns], as.numeric)
# library(MASS)
#  
# crit_2_2_C_data_raw <- undernourishment %>%
#   rowwise() %>%
#   mutate(undernourishment_mean = mean(c_across(year_columns)/100, na.rm = TRUE)) %>%
#   ungroup() %>%  mutate(Continent = countrycode(
#     country_ISO_alpha3, 
#     origin = "iso3c", 
#     destination = "continent"
#   )) %>% filter(Continent!="NA") %>% 
#   filter(
#     !rowSums(is.na(.)) > 3  # Keep countries with 2 or fewer missing values
#   ) 
# 
# crit_2_2_C_continent_data<- crit_2_2_C_data_raw %>% 
#   group_by(Continent) %>%
#   summarize(
#     continent_mean = mean(undernourishment_mean, na.rm = TRUE)
#   )
# 
# 
# # Merge the continent averages back into the data and impute missing values
# crit_2_2_C_data <- crit_2_2_C_data_raw %>%
#   left_join(crit_2_2_C_continent_data, by = "Continent") %>%
#   mutate(  crit_2_2_C_raw = ifelse(is.na(undernourishment_mean), continent_mean, undernourishment_mean) ) %>% #impute missing values
#   mutate(crit_2_2_C_log = log( crit_2_2_C_raw +1)) %>% 
#   mutate(crit_2_2_C = scales::rescale(crit_2_2_C_log))
# 
# 
# # plot
# ggplot(crit_2_2_C_data) +
#   geom_histogram( aes(x = crit_2_2_C_raw))+
#   geom_histogram( aes(x =crit_2_2_C), fill="transparent",color="pink")
# 
# shapiro.test(crit_2_2_C_data$crit_2_2_C)


#add grouped bars by CONTINENTS
selected_columns <- c("ID", "index_prod_scaled", "FAO.Region")
# Subset the data frame
df_subset <- df_spatial[selected_columns]
df_subset$Country <- factor(df_subset$ID, levels = unique(df_subset$ID[order(df_subset$FAO.Region)]))
region_order <- c( "Europe/Asia", "Europe","Oceania","Americas","Asia", "Africa" )
# Order the levels of FAO.Region
df_subset$FAO.Region <- factor(df_subset$FAO.Region, levels = region_order)
df_subset$ID<- factor(df_subset$ID, levels = unique(df_subset$ID[order(df_subset$FAO.Region, df_subset$index_prod_scaled)]))


P<-ggplot(df_subset, aes(x = ID, y = index_prod_scaled,fill = index_prod_scaled)) +
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
region_anova_result <- aov(index_prod_scaled ~ FAO.Region, data = df_spatial)
p_value_region <- summary(region_anova_result)[[1]]$`Pr(>F)`[1]

# Print the ANOVA result
summary(region_anova_result)
region_order <- c(  "Africa" , "Asia", "Oceania","Europe/Asia","Americas","Europe")
# Order the levels of FAO.Region
df_spatial$FAO.Region <- factor(df_spatial$FAO.Region, levels = region_order)

region_compared<-
  ggplot(df_spatial, aes(y = index_prod_scaled, x = FAO.Region)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="A",y = "Vulnerability", x = "Region") +
  theme_classic()+
  annotate("text", x = 3, y = max(df_spatial$index_prod_scaled) * 0.95, 
           label = ifelse(t_test_tropical$p.value > 0.01, "p > 0.01", ifelse(t_test_tropical$p.value < 0.001, "p < 0.001", paste("p =", signif(t_test_tropical$p.value, digits = 3)))),
           hjust = 1, size = 4)+
  theme(text = element_text(size = 14))

region_compared


df_spatial$LeastDeveloped_2018 <- factor(ifelse(is.na(df_spatial$LeastDeveloped_2018) | df_spatial$LeastDeveloped_2018 == "no", "no", "yes"), levels = c("yes", "no"))
t_test_LDC <- t.test(index_prod_scaled ~ LeastDeveloped_2018, data = df_spatial)

LDC_compared<-ggplot(df_spatial, aes(y = index_prod_scaled,x = LeastDeveloped_2018)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="B", x = "Development level", y = "Vulnerability") +
  theme_classic() +
  theme(text = element_text(size = 14))+
  scale_x_discrete(labels = c("yes" = "LDC", "no" = "Non-LDC"))+
  annotate("text", x = 1.8, y = max(df_spatial$index_prod_scaled) * 0.95, 
           label = ifelse(t_test_LDC$p.value > 0.01, "p > 0.01", ifelse(t_test_LDC$p.value < 0.001, "p < 0.001", paste("p =", signif(t_test_tropical$p.value, digits = 3)))),
           hjust = 1, size = 4)


#check for effect of tropcial 
df_spatial <- df_spatial[complete.cases(df_spatial$Tropical), ]

df_spatial <- df_spatial %>%
  mutate(Tropical = ifelse(Proportion_TropicsZone > 0.75, "Tropical", "Not Tropical"))
df_spatial$Tropical <- factor(df_spatial$Tropical, levels = c("Tropical", "Not Tropical"))


#t test
t_test_tropical <- t.test(index_prod_scaled ~ Tropical, data = df_spatial)

tropical_compared<- ggplot(df_spatial, aes(y = index_prod_scaled,x = Tropical)) +
  geom_jitter(color="lightgrey")+
  geom_boxplot(fill="transparent",outlier.shape = NA) +
  labs(title="C", y = "Vulnerability", x = "Country type") +
  theme_classic() +
  annotate("text", x = 1.8, y = max(df_spatial$index_prod_scaled) * 0.95, 
           label = ifelse(t_test_tropical$p.value > 0.01, "p > 0.01", 
                          ifelse(t_test_tropical$p.value < 0.001, "p < 0.001",
                                 paste("p =", signif(t_test_tropical$p.value, digits = 3)))),
           hjust = 1, size = 4)+
  theme(text = element_text(size = 18))

tropical_compared

boxplots<-region_compared+(LDC_compared+tropical_compared )
boxplots

ggsave("boxplots.tiff", dpi=300, height=3, width=12)



A<-  ggplot() +
  geom_sf(data = world_shp_iso , fill = "gray", color = "lightgrey") +
  geom_sf(data=df_spatial, aes(fill = index_mean_scaled), color = "lightgrey")+
  color_scale+
  labs(fill = "Vulnerability Index") +
  theme_classic() +
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))+
  geom_hline(yintercept = 0,  color = "darkgrey", alpha = 0.5,  linewidth = 0.2) +  # Equator
  geom_hline(yintercept = 23.5, linetype = "dashed",color = "darkgrey", alpha = 0.8, linewidth=0.2) +  # Tropic of Cancer
  geom_hline(yintercept = -23.5, linetype = "dashed", color = "darkgrey", alpha = 0.8, linewidth = 0.2)  # Tropic of Capricorn
A
ggsave("vulnerability_map.tiff", dpi=300, height=3, width=6)


# Create a bar chart for the top 20 countries
bars<-df %>%
  arrange(desc(index_prod_scaled)) %>%
  ggplot(aes(x = reorder(Country, -index_prod_scaled), y = index_prod_scaled, fill = index_prod_scaled)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(x = "Country", y = "Index") +
  theme_classic() +
  theme(text=element_text(size=13))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
bars

ggsave("country_scores_plot.tiff", dpi=300, width=19, height=5)

# Assign continents to countries using the countrycode package
df_continents <- df_new_named %>%
  mutate(Continent = countrycode(
    Alpha.3.code, 
    origin = "iso3c", 
    destination = "continent"
  ))


# Calculate the average score by continent
average_scores <- df_continents %>%
  group_by(Continent) %>%
  summarise(Average_Score = mean(index_prod_scaled, na.rm = TRUE))

# Create a bar plot of the average scores by continent
continent_plot <- ggplot(average_scores, aes(x = reorder(Continent, Average_Score), y = Average_Score)) +
  geom_bar(stat = "identity") +
  # scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(x = "Continent", y = "Mean Vulnerability") +
  theme_classic() +
  coord_flip() +
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(text=element_text(size=25))

# Display the bar plot
continent_plot

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


#EXPOSURE MODEL ########
model_E <- '
# Latent Variable Definition
  
  E=~  crit_1_2+ crit_1_1_A +crit_1_1_B
  # Adding covariances based on modification indices
   # crit_1_1_A ~~ crit_1_1_B'

# Run the CFA
cfa_model_E <- cfa(model_E, data=df, estimator="MLM")

# Check the fit of the model
summary(cfa_model_E, fit.measures=TRUE)
fit_measures_E <- fitMeasures(cfa_model_E, fit.measures = desired_measures)
fit_measures_E

vartable(cfa_model_E)

# Plot the CFA results
semPaths(cfa_model_E, what = "std",
         curvePivot = TRUE,layout = "tree",  style = "ram",
         #     nodeLabels = c(  "SSF production","nearshore LSF", "Exposure"),
         sizeMan = 11, sizeLat = 10,sizeLat2=10, sizeMan2=10,sizeInt2=15,
         edge.color = ifelse(lavInspect(cfa_model_E, "std")$lambda > 0, "royalblue", "red3"), # Change colors based on sign
         nCharNodes = 0.2, # To reduce the space between nodes, set to 0 or adjust as necessary
         edge.label.cex = 0.75)
#dev.off()
lavInspect(cfa_model_E, "cov.ov")
mod_indices <- modificationindices(cfa_model_E)
print(mod_indices, sort = TRUE)

# Plot the CFA results
factor_loadings_e <- lavInspect(cfa_model_E, "std.lv")
# Extract factor names and loadings separately
factor_names <- colnames(factor_loadings_e[[1]])
loadings <- as.vector(factor_loadings_e[[1]])

# Create a data frame
loadings_df_e <- as.data.frame(as.table(factor_loadings_e$lambda))
colnames(loadings_df_e) <- c("variable", "factor", "loading")

factor_loadings_e_plot<-ggplot(loadings_df_e, aes(x = variable, y = loading)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Exposure", x = "Factor", y = "Loading") +
  theme_minimal() +
  scale_fill_continuous(limits=c(-1,1.5))+
  #scale_x_discrete(labels=c(  "SSF production", "nearshore LSF" ))+
  coord_flip()+
  theme(legend.position = "none")+
  theme(text=element_text(size=14))

factor_loadings_e_plot

# Calculate the factor scores
factor_scores_e <- lavPredict(cfa_model_E, type = "lv")

# Add factor scores to your dataframe
df_cfa$cfa_exposure_score_e <- factor_scores_e

cfa_comparison_exposure<-df_cfa %>% 
  ggplot(aes(x=fct_reorder(iso_code, cfa_exposure_score_e), y=cfa_exposure_score_e ,fill= cfa_exposure_score_e))+
  geom_col()+
  scale_fill_viridis(direction=-1, option="magma")+
  coord_flip()+
  theme_classic()+
  labs(x="Country", y="CFA E score", fill = "E score")

cfa_comparison_exposure



#SENSITIVITY MODEL #########
model_S<- '

S =~   crit_2_1_B + crit_2_1_A + crit_2_1_C + crit_2_1_D
S =~ crit_2_2_A +crit_2_2_C + crit_2_2_B


# covariances
crit_2_1_C ~~ crit_2_2_B
crit_2_1_D ~~ crit_2_2_C
crit_2_1_C ~~ crit_2_2_A
 #crit_2_1_B ~~ crit_2_2_B
 #crit_2_1_B ~~ crit_2_2_D
 


'

# Run the CFA
cfa_model_S <- cfa(model_S, data=df, estimator="MLMV")
# Check the fit of the model
summary(cfa_model_S, fit.measures=TRUE)
fit_measures_S <- fitMeasures(cfa_model_S, fit.measures = desired_measures)
fit_measures_S

vartable(cfa_model_S)

# Plot the CFA results
#png("/Users/melissacronin/Desktop/March_IHH_figures/sensitivity_cfa_sem_plot.png", width = 1600, height = 1200, res = 600) # Adjust size and resolution as needed

semPaths(cfa_model_S, what = "std",
         curvePivot = TRUE,layout = "tree",  style = "ram",
         # nodeLabels = c( "SSF labor force",
         #                 "SSF landed value", 
         #                 "SSF employment",
         #                 "SSF subsistence",
         #                 "SSF coastal food", 
         #                 "Micronutrient need",  "SSF nutrition", 
         #                 "Sensitivity"),
         sizeMan = 11, sizeLat = 25,sizeLat2=10, sizeMan2=10,sizeInt2=15,
         edge.color = ifelse(lavInspect(cfa_model_S, "std")$lambda > 0, "royalblue", "red3"), # Change colors based on sign
         nCharNodes = 0.2, # To reduce the space between nodes, set to 0 or adjust as necessary
         edge.label.cex = 0.75)

#dev.off()

lavInspect(cfa_model_S, "cov.ov")
mod_indices <- modificationindices(cfa_model_S)
print(mod_indices, sort = TRUE)


# Get the standardized solution
std_loadings <- standardizedSolution(cfa_model_S)

# Print the standardized factor loadings
print(std_loadings)

# Plot the CFA results
factor_loadings_s <- lavInspect(cfa_model_S, "std.lv")
# Extract factor names and loadings separately
factor_names_s <- colnames(factor_loadings_s[[1]])
loadings_s <- as.vector(factor_loadings_s[[1]])

# Create a data frame
loadings_df_s <- as.data.frame(as.table(factor_loadings_s$lambda))
colnames(loadings_df_s) <- c("variable", "factor", "loading")

factor_loadings_s_plot<- ggplot(loadings_df_s, aes(x = variable, y = loading)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sensitivity", x = "Factor", y = "Loading") +
  theme_minimal() +
  # scale_fill_continuous(limits=c(-1.5,1.5))+
  scale_x_discrete(labels=c(  "SSF nutrition", "SSF need", "SSF production", 
                              "SSF subsistence", "SSF labor force","SSF value",
                              "SSF employment"))+
  coord_flip()+
  theme(legend.position = "none")+
  theme(text=element_text(size=14))

factor_loadings_s_plot



#ADAPTIVE CAPCACITY MODEL ########
model_AC<-  '
AC=~  
crit_3_1_inverse + crit_3_2_A_inverse + crit_3_2_B_inverse + crit_3_2_C_inverse + crit_3_2_D_inverse+ crit_3_2_E_inverse + crit_3_2_F_inverse
# Added covariances based on modification indices
# crit_3_1_inverse_raw ~~ crit_3_2_B_inverse_raw
# crit_3_2_A_inverse_raw ~~ crit_3_2_C_inverse_raw
# crit_3_2_B_inverse_raw ~~ crit_3_2_D_inverse_raw
'

# Run the CFA
cfa_model_AC <- cfa(model_AC, data=df, estimator="MLR")
# Check the fit of the model
summary(cfa_model_AC, fit.measures=TRUE)
fit_measures_AC <- fitMeasures(cfa_model_AC, fit.measures = desired_measures)
fit_measures_AC

# Plot the CFA results
#png("/Users/melissacronin/Desktop/March_IHH_figures/adaptive_capacity_cfa_sem_plot.png", width = 1600, height = 1200, res = 600) # Adjust size and resolution as needed

semPaths(cfa_model_AC, what = "std",
         curvePivot = TRUE,layout = "tree",  style = "ram",
         nodeLabels = c( "HDI","Corruption","Political stability",
                         "Government effectiveness","Regulatory quality","Rule of law","Voice + accountability","Adaptive capacity"),
         sizeMan = 11, sizeLat = 25,sizeLat2=10, sizeMan2=10,sizeInt2=15,
         edge.color = ifelse(lavInspect(cfa_model_AC, "std")$lambda > 0, "royalblue", "red3"), # Change colors based on sign
         nCharNodes = 0.2, # To reduce the space between nodes, set to 0 or adjust as necessary
         edge.label.cex = 0.75)

#dev.off()
lavInspect(cfa_model_AC, "cov.ov")
mod_indices <- modificationindices(cfa_model_AC)
print(mod_indices, sort = TRUE)


# Plot the CFA results
factor_loadings_ac <- lavInspect(cfa_model_AC, "std.lv")

# Extract factor names and loadings separately
factor_names_ac <- colnames(factor_loadings_ac[[1]])
loadings_ac <- as.vector(factor_loadings_ac[[1]])

# Create a data frame
loadings_df_ac <- as.data.frame(as.table(factor_loadings_ac$lambda))
colnames(loadings_df_ac) <- c("variable", "factor", "loading")

factor_loadings_ac_plot<-ggplot(loadings_df_ac, aes(x = variable, y = loading)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Adaptive capacity", x = "Factor", y = "Loading") +
  theme_minimal() +
  scale_x_discrete(labels=c(  "HDI", "corruption", "political stability","gov. effectiveness", "regulatory quality", "rule of law", "voice/accountability"))+
  coord_flip()+
  theme(legend.position = "none")+
  theme(text=element_text(size=14))

(factor_loadings_v_plot + factor_loadings_e_plot )/(factor_loadings_s_plot + factor_loadings_ac_plot )
ggsave("factor_loadings.tiff", dpi=300, height=8, width=10)


# Combine factor loadings data frames into a list
factor_loadings_list <- list(loadings_df_v,loadings_df_e, loadings_df_s, loadings_df_ac)


# Create an empty data frame to store all factor loadings
all_loadings <- data.frame()

# Loop through each factor loadings data frame
for (i in 1:length(factor_loadings_list)) {
  # Append the factor loadings to the combined data frame
  all_loadings <- rbind(all_loadings, factor_loadings_list[[i]][, c("factor", "variable", "loading")])
}



# Print the table
print(all_loadings)
write.csv(all_loadings, "factor_loading_scores.csv")

####CFA ######
#dont use chi sq values  
#CFI - values .95 and above are good, .90 are acceptabel fit. If lower=lack of fit.
#TLI (or non nrom fit)
#Root mean sq error (RMSEA) of 0.05 or below are considered close fit. non-sig p value indicates close fit
#SRMR of 0.05 or below = close fit model

# CFA: Latent Factors - unobserved constructs or factors that you are hypothesizing to exist
# Observed Variables - variables that you have measured or observed directly. They are indicators of the latent factors.


# ##CREATE MAPS ######
# 
# world_shp_iso <- world_shp %>%
#   mutate(Alpha.3.code = countrycode(sourcevar = ID, origin = "country.name", destination = "iso3c"))
# 
# df<-df %>%  rename(Alpha.3.code =country_ISO_alpha3)
# df_spatial <- merge(world_shp_iso, df,by="Alpha.3.code" )
# 
# ####
# 
# # Create a bar chart of the top 20 ranking exposure countries
# top_20_vulnerability <- df_spatial %>% 
#   arrange(desc(index_mean_scaled)) %>% 
#   slice(1:20)
# 
# bar_A <- ggplot(top_20_vulnerability, aes(x = reorder(Alpha.3.code, index_prod_scaled), y = index_prod_scaled, fill=index_prod_scaled)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(expand=c(0,0))+
#   coord_flip() +
#   labs(x = NULL, y = "Vulnerability") +
#   color_scale +  # Use the same scale as in the map
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position="none")+
#   theme(
#     text = element_text(size = 16),  
#     axis.text = element_text(size = 14) )
# 
# bar_A
# 
# 
# ###### EXPOSURE
# 
# exposure_map<-
#   ggplot() +
#   geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
#   geom_sf(data = df_spatial , aes(fill = exposure_scaled), color = "darkgray") +
#   color_scale + 
#   labs( fill = "Exposure") +
#   theme_bw()+
#   theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))
# 
# exposure_map
# 
# 
# # Create a bar chart of the top 20 ranking exposure countries
# top_20_exposure <- df_spatial %>% 
#   arrange(desc(exposure_scaled)) %>% 
#   slice(1:20)
# 
# bar_exposure <- ggplot(top_20_exposure, aes(x = reorder(Alpha.3.code, exposure_scaled), y = exposure_scaled, fill=exposure_scaled)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(expand=c(0,0))+
#   coord_flip() +
#   labs(x = NULL, y = "Exposure") +
#   color_scale +  # Use the same scale as in the map
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position="none")+
#   theme(
#     text = element_text(size = 16),  
#     axis.text = element_text(size = 14) )
# 
# bar_exposure
# 
# 
# sensitivity_map<-
#   ggplot() +
#   geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
#   geom_sf(data=df_spatial, aes(fill =sensitivity_scaled), color = "darkgray") +
#   color_scale+
#   labs( fill = "Sensitivity") +
#   theme_bw()+
#   theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))
# 
# sensitivity_map
# 
# 
# # Create a bar chart of the top 20 ranking countries
# top_20_sensitivity <- df_spatial %>% 
#   arrange(desc(sensitivity_scaled)) %>% 
#   slice(1:20)
# 
# sensitivity_bar <- ggplot(top_20_sensitivity, aes(x = reorder(Alpha.3.code, sensitivity_scaled), y = sensitivity_scaled, fill=sensitivity_scaled)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(expand=c(0,0))+
#   coord_flip() +
#   labs(x = NULL, y = "Sensitivity") +
#   color_scale +  # Use the same scale as in the map
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position="none")+
#   theme(
#     text = element_text(size = 16),  
#     axis.text = element_text(size = 14) )
#   
# sensitivity_bar 
# 
# 
# adaptive_capacity_map<- ggplot() +
#   geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
#   geom_sf(data=df_spatial, aes(fill =adaptive_capacity_scaled), color = "darkgray") +
#   scale_fill_viridis_c(option = "magma", direction = 1, na.value = "gray", limits = color_range)+
#   labs( fill = "Adaptive capacity") +
#   theme_bw()+
#   theme(legend.position = "bottom", legend.text = element_text(size = 8), legend.title = element_text(size = 10))
# 
# adaptive_capacity_map
# 
# # Create a bar chart of the top 20 ranking exposure countries
# top_20_adaptive_capacity <- df_spatial %>% 
#   arrange((adaptive_capacity_inverse_scaled)) %>% 
#   slice(1:20)
# 
# adaptive_capacity_bar <- ggplot(top_20_adaptive_capacity, aes(x = reorder(Alpha.3.code, adaptive_capacity_scaled), y = adaptive_capacity_scaled, fill=adaptive_capacity_scaled)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(expand=c(0,0))+
#   coord_flip() +
#   labs(x = NULL, y = "Adaptive Capacity") +
#   scale_fill_viridis_c(option = "magma", direction = 1, na.value = "gray", limits = color_range)+
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position="none")+
#   theme(
#     text = element_text(size = 16),  
#     axis.text = element_text(size = 14) )
# 
# adaptive_capacity_bar 
# 
# 
# 
# 
# 
# # 
# # 
# # #OTHER PLOTS #######
# # #exploratory analyses of relationships between components
# # 
# # # Set a threshold value for displaying labels
# # threshold_value <- 0.8  # Adjust this value to your preference
# # 
# # # Create bubble plot to show relationship between exposure and s, ac
# # plot_data_lm<- df %>% 
# #  # filter(Alpha.3.code!="CHN") %>% 
# #   mutate(mean_sensitivity_adaptive = rowMeans(cbind(sensitivity_scaled, adaptive_capacity_inverse_scaled)))
# # 
# # linear_model <- lm(exposure_scaled ~ mean_sensitivity_adaptive, data = plot_data_lm)
# # summary(linear_model)
# # 
# # Extract p-value for the slope coefficient
# p_value_slope <- summary(linear_model)$coefficients["mean_sensitivity_adaptive", "Pr(>|t|)"]
# 
# 
# mean_S_AC<-plot_data_lm%>%
#   ggplot(aes(y = exposure_scaled, x = mean_sensitivity_adaptive)) +
#   geom_point(aes(alpha=0.8), size=4) +
#   labs(y = "Exposure", x  = "Mean(Adaptive capacity, Sensitivity)" , title="Exposure - Mean(S, AC)") +
#   theme_classic()+
#   geom_smooth(method="lm", color="navy", fill = "navy")+
#   theme(legend.position="none")+
#   theme(text=element_text(size=16))+
#   scale_y_continuous(limits=c(0,1))+
#   annotate("text", x = 0.3, y = 0.9, 
#            label = paste("Slope: ", round(coef(linear_model)[2], 4), "\nIntercept: ", round(coef(linear_model)[1], 4), "\np-value: ", format(p_value_slope, scientific = TRUE)), 
#        color = "black")
# mean_S_AC
# 
# 
# #senstivity alone 
# linear_model_S <- lm(exposure_scaled ~ sensitivity_scaled, data =  df)
# summary(linear_model_S)
# 
# # Extract p-value for the slope coefficient
# p_value_slope_S <- summary(linear_model_S)$coefficients["sensitivity_scaled", "Pr(>|t|)"]
# 
# sensitivity_plot<- df %>%
#   ggplot(aes(y = exposure_scaled, x = sensitivity_scaled)) +
#   geom_point(aes(alpha=0.8), size=4) +
#   labs(y = "Exposure", x  = "Sensitivity", title="Exposure - Sensitivity") +
#   theme_classic()+
#   geom_smooth(method="lm", color="navy", fill = "navy")+
#   theme(legend.position="none")+
#   theme(text=element_text(size=16))+
#   annotate("text", x = 0.3, y = 0.9, 
#            label = paste("Slope: ", round(coef(linear_model_S)[2], 4), "\nIntercept: ", round(coef(linear_model_S)[1], 4), "\np-value: ", format(p_value_slope_S, scientific = TRUE)), 
#            color = "black")
# 
# sensitivity_plot
# 
# linear_model_AC <- lm(exposure_scaled ~ adaptive_capacity_inverse_scaled, data =  df)
# summary(linear_model_AC)
# 
# # Extract p-value for the slope coefficient
# p_value_slope_AC <- summary(linear_model_AC)$coefficients["adaptive_capacity_inverse_scaled", "Pr(>|t|)"]
# 
# AC_plot<-df %>%
#   ggplot(aes(y = exposure_scaled, x = adaptive_capacity_inverse_scaled)) +
#   geom_point(aes(alpha=0.8), size=4) +
#   labs(y = "Exposure", x  = "Adaptive Capacity", title="Exposure - Adaptive Capacity") +
#   theme_classic()+
#   geom_smooth(method="lm", color="navy", fill = "navy")+
#   theme(legend.position="none")+
#  # geom_text(aes(label = Alpha.3.code), vjust = -0.5, hjust = 0.5, size = 3) + 
#   theme(text=element_text(size=16))+
#   annotate("text", x = 0.3, y = 0.9, 
#            label = paste("Slope: ", round(coef(linear_model_AC)[2], 4), "\nIntercept: ", round(coef(linear_model_AC)[1], 4), "\np-value: ", format(p_value_slope_AC, scientific = TRUE)), 
#            color = "black")
# AC_plot
# 
# mean_S_AC + sensitivity_plot + AC_plot
# 

