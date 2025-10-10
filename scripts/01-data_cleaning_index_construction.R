###############################################################################
# SSF Dependence Country Prioritization using IHH Data
# 
# Description: 
#   This script cleans and standardizes all SIFI Index data and constructs the SIFI Index.
#     It is part of the SSF-LSF Interactions project.
#
# Author: Melissa Cronin
# Created: 2024-06-14
# Last Updated: 2025-09-24
#
# Inputs:
#   - data/raw/ihh_data.csv
#   - data/processed/country_metadata.csv
#
# Outputs:
#   - data/processed/TKTK .csv
#   - outputs/figures/TKTK
#
# Run Order:
#   This is script 01 in the workflow:
#     01_data_cleaning.R  →  02_analysis_and_visualization.R
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

# COMPONENT 1. EXPOSURE ########
###############################################################################

### 1_1 Construct composite index of intensity of nearshore large-scale fishing activity 3####

# 1. Read CSV 
gfw_data <- read.csv(
  here("data", "gfw_fishing_SAR_matched.csv")) %>%  
  filter(country_name!="") %>% #remove eez null fishing
  dplyr::select(country_name, matched_fishing, unmatched_fishing)

#1_1_B Ratio of undetected fishing to detected fishing
#Ratio of LSF fishing activity that is not publicly tracked (AIS activity is missing from public monitoring systems) to fishing activity that is publicly tracked with 25 km of shore

gfw_data$Alpha.3.code <- countrycode(gfw_data$country_name, "country.name", "iso3c")

countries_subset <- c( #the package missed.a few codes, so manually input them 
  "Bonaire" = "BES",
  "Saint Martin" = "MAF") 

#create crit_1_1_A : Matched LSF density AIS-matched vessel detections

crit_1_1_A_data <- gfw_data %>%
  mutate(Alpha.3.code = if_else(country_name %in% names(countries_subset), 
                                countries_subset[country_name], 
                                Alpha.3.code)) %>% 
  rename(crit_1_1_A_raw= matched_fishing) %>% 
  mutate(crit_1_1_A_log  = log(crit_1_1_A_raw+1)) %>% 
  mutate(crit_1_1_A  =scales::rescale(crit_1_1_A_log ))

crit_1_1_A_data %>% 
  ggplot()+
  geom_histogram(aes(x=crit_1_1_A))



#create crit_1_1_B : Non-matched (eg non-broadcasting) LSF density vessel detections
crit_1_1_B_data <- gfw_data %>%
  mutate(Alpha.3.code = if_else(country_name %in% names(countries_subset), 
                                countries_subset[country_name], 
                                Alpha.3.code)) %>% 
  rename(crit_1_1_B_raw= unmatched_fishing) %>% 
  mutate(crit_1_1_B_log  = log(crit_1_1_B_raw+1)) %>% 
  mutate(crit_1_1_B  =scales::rescale(crit_1_1_B_log ))


crit_1_1_B_data %>% 
ggplot()+
  geom_histogram(aes(x=crit_1_1_B))

#create crit_1_1 : LSF density: mean (AIS-matched and non-matched vessel detections)
crit_1_1_data<- crit_1_1_A_data %>% 
  left_join(crit_1_1_B_data, by = c("Alpha.3.code", "country_name")) %>%
  as.data.frame() %>% 
  drop_na() %>% 
  mutate(crit_1_1 = rowMeans(dplyr::select(.,crit_1_1_A, crit_1_1_B) )) 
  

crit_1_1_histogram<-crit_1_1_data %>% 
  ggplot()+
  geom_histogram(aes(x=crit_1_1))

shapiro.test(crit_1_1_data$crit_1_1)

cor(crit_1_1_data$crit_1_1_A_raw,crit_1_1_data$crit_1_1_B_raw, ) #check correlation between LSF variables

### 1_2_A Contribution to global marine SSF production (scaled to nearshore area , IHH) #####
#Description: Percent contribution of countries’ small-scale fisheries production to global small-scale fisheries production

#first, need a calculation of square km of area within 25km for every country, using a buffer created in GIS
# World polygons from the maps package
world_shp <- sf::st_as_sf(maps::map("world", plot = F, fill = TRUE))

world_shp_df <- world_shp %>%
  mutate(Alpha.3.code = countrycode(ID, "country.name", "iso3c", warn = FALSE))

coastal_buffer <- st_read(here("data","buffered_25km_GIS_files","resultLayer.shp")) %>%
  filter(sum_Area_S >= 1) %>%
  mutate(eez = str_replace(eez, " Exclusive Economic Zone", ""))

# Add a column with ISO country codes
coastal_buffer$iso_code <- countrycode(
  sourcevar = coastal_buffer$eez,
  origin = "country.name",
  destination = "iso3c"
) 

coastal_buffer<- coastal_buffer%>% 
  dplyr::select(eez, sovereign, iso_code,sum_Area_S)

#manually fix country codes for territories
countries_subset <- c(
  "Alaskan" = "USA",
  "Amsterdam Island & St. Paul Island" = "ATF",
  "Andaman and Nicobar Islands" = "IND",
  "Ascension" = "SHN",
  "Azores" = "PRT",
  "Belgian" = "BEL",
  "Bermudian" = "BMU",
  "Bornholm" = "DNK",
  "Canadian" = "CAN",
  "Canary Islands" = "ESP",
  "Chinese" = "CHN",
  "Clipperton Island" = "FRA",
  "Comoran" = "COM",
  "Conflict Zone" = "Disputed",
  "Crozet Islands" = "ATF",
  "Cypriote" = "CYP",
  "Danish" = "DNK",
  "Dominican" = "DMA",
  "Dutch" = "NLD",
  "Easter Island" = "CHL",
  "Finnish" = "FIN",
  "French" = "FRA",
  "Galapagos" = "ECU",
  "German" = "DEU",
  "Glorioso" = "FRA",
  "Grecian" = "GRC",
  "Grenadian" = "GRD",
  "Guadeloupe & Martinique" = "GLP",
  "Guyanese" = "GUY",
  "Hawaiian" = "USA",
  "Honduran" = "HND",
  "Howland and Baker Island" = "UMI",
  "Ile Europa" = "FRA",
  "Ile Tromelin" = "FRA",
  "Irish" = "IRL",
  "Italian" = "ITA",
  "Jan Mayen" = "NOR",
  "Jarvis Island" = "UMI",
  "Johnston Atoll" = "UMI",
  "Juan de Nova" = "FRA",
  "Kerguelen Islands" = "ATF",
  "Lebanese" = "LBN",
  "Line Group" = "KIR",
  "Jan Mayen"= "SJM",
  "Macquarie Island" = "AUS",
  "Madagascan" = "MDG",
  "Madeiran" = "PRT",
  "Maltese" = "MLT",
  "Mauritian" = "MUS",
  "Micronesian" = "FSM",
  "Monégasque" = "MCO",
  "Moroccan" = "MAR",
  "Mozambican" = "MOZ",
  "Netherlands Antilles" = "ABW",
  "Northern Mariana Islands and Guam" = "MNP",
  "Norwegian" = "NOR",
  "Oecussi Ambeno" = "TLS",
  "Palmyra Atoll" = "UMI",
  "Paracel Islands" = "Disputed",
  "Phoenix Group" = "KIR",
  "Polish" = "POL",
  "Portuguese" = "PRT",
  "Prince Edward Islands" = "ZAF",
  "Puerto Rican" = "PRI",
  "Russia-Japan conflict zone" = "Disputed",
  "Saint-Martin" = "MAF",
  "Serbia-Montenegrian" = "MNE",
  "Somali" = "SOM",
  "Spratly Island"= "Disputed",
  "Swedish" = "SWE",
  "Turkish" = "TUR",
  "Virgin Islands" = "VIR",
  "Wake Island" = "UMI"
)

# Create a data frame with country names and ISO codes
iso_mapping_df <- data.frame(
  eez = names(countries_subset),
  iso_code = unname(countries_subset)
)

coastal_buffer_joined <- left_join(coastal_buffer, iso_mapping_df, by = "eez",  relationship = "many-to-many")

# Use coalesce to fill missing iso_code values with iso_code.y
coastal_buffer_joined <- coastal_buffer_joined %>%
  mutate(iso_code = coalesce(iso_code.x, iso_code.y)) %>%
  dplyr::select(-iso_code.x, -iso_code.y)  # Remove redundant columns

coastal_buffer_final<- coastal_buffer_joined %>% 
  group_by(iso_code) %>% 
  summarise(sum_area=sum(sum_Area_S))

coastal_buffer_df<- coastal_buffer_final %>% 
  st_drop_geometry()

#write.csv(coastal_buffer_df, "coastal_buffer_iso.csv")

ggplot() +
  geom_sf(data = world_shp, fill = "lightgray", color = "darkgray") +
  geom_sf(data = coastal_buffer_final, aes(color = sum_area)) +
  scale_color_viridis() +
  labs( color="Nearshore area (km^2)") +
  theme_classic()


#now add ssf catch data, to be scaled relative to nearshore area for each country
#THIS DATA IS CONFIDENTIAL AND CANNOT BE SHARED. Calculation details remain here to clarify how the data was used in the analysis.  

ssf_catch<- read.csv("ssf_catch.csv", sep=",", header=TRUE) %>% 
  filter(Marine_Inland_char=="Marine") %>% 
  group_by(country, country_ISO_alpha3) %>% 
  slice_head(n=1) %>%
  dplyr::select(country,national_catch_final ) %>% 
  ungroup() %>% 
  drop_na()

ssf_catch$national_catch_final<-round(ssf_catch$national_catch_final)

nearshore_area<-coastal_buffer_final %>%  rename(country_ISO_alpha3=iso_code)

ssf_catch_area<- ssf_catch %>% 
  left_join( nearshore_area, by ="country_ISO_alpha3")

crit_1_2_data<- ssf_catch_area %>% 
  mutate(catch_per_area= log(national_catch_final)/log(sum_area)) %>% 
  rename(crit_1_2_raw= catch_per_area) %>% 
  mutate( crit_1_2 = scales::rescale(crit_1_2_raw, to = c(0, 1)) )%>% 
  rename(Alpha.3.code=country_ISO_alpha3) %>% 
  filter(crit_1_2!="NA")

# Plot a histogram of normalized_crit_2_1_A
crit_1_2_histogram<-ggplot(crit_1_2_data) +
  geom_histogram(aes(x = crit_1_2)) 

crit_1_2_df<- crit_1_2_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(-geometry)

shapiro.test(crit_1_2_df$crit_1_2)

### Aggregate exposure variables #####

### CALCULATE EXPOSURE ######

exposure_data_df<- crit_1_1_data %>% 
  left_join(crit_1_2_data, by =c( "Alpha.3.code")) %>% 
  drop_na() %>% 
  mutate(exposure_raw = rowMeans(dplyr::select(., crit_1_1, crit_1_2 ), na.rm = TRUE)) %>% 
  mutate(exposure_scaled=scales::rescale(exposure_raw)) %>% 
  dplyr::select(Alpha.3.code,
                #country, hours, vessel_density,
                #crit_1_1_raw,crit_1_1_A_raw,crit_1_1_B_raw, crit_1_2_raw, 
                crit_1_1,crit_1_1_A,crit_1_1_B, crit_1_2 ,exposure_scaled, exposure_raw) 

shapiro.test(exposure_data_df$exposure_raw)

ggplot(exposure_data_df) +
  geom_histogram(aes(x = exposure_raw)) 

joined_exp  <- merge(exposure_data_df, world_shp_df, by = "Alpha.3.code")
exposure_data<- st_as_sf(joined_exp) 

#check correlations
cor(exposure_data_df$crit_1_1_A,exposure_data_df$exposure_raw)

ggplot(exposure_data_df) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = crit_1_1), fill = "pink", position = "dodge") +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = -crit_1_2), fill = "blue", position = "dodge") +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = exposure_scaled) ,fill = "green", position = "dodge") +
  coord_flip()

ggplot(exposure_data_df) +
  geom_point(aes(x =  crit_1_1, y = crit_1_2, color = exposure_scaled ))
  

sf_palette <- viridis::viridis(7, option = "inferno", direction=-1)

exposure_map<-exposure_data %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf(data=exposure_data, aes( fill= exposure_scaled), color="lightgrey") +
  scale_fill_gradientn(colors = sf_palette  )+ 
  labs(title = "Exposure") +
  theme(legend.position="top")+
  theme_classic()



A<-exposure_data %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf(data=exposure_data, aes( fill= crit_1_1_A), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,  limits=c(0,1), breaks=c(0,0.5,1))+
  labs( fill="Nearshore density of matched vessels (broadcasting AIS)") +
  theme_classic()+
  theme(legend.position="top",
        legend.key.size = unit(0.5, "cm"))
A
B<-exposure_data %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf(data=exposure_data, aes( fill= crit_1_1_B), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1, limits=c(0,1), breaks=c(0,0.5,1))+
  labs( fill= "Nearshore density of unmatched vessels (not broadcasting AIS)") +
  theme_classic()+
  theme(legend.position="top",
        legend.key.size = unit(0.5, "cm"))  # Adjust plot margins
 
B

C<-exposure_data %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf(data=exposure_data, aes( fill= crit_1_2), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,limits=c(0,1), breaks=c(0,0.5,1))+
  labs(fill="SSF catch (kg) / nearshore area km^2") +
  theme_classic()+  
  theme(legend.position="top")
C
(A/B)+ C

#ggsave("Fig_2_exposure.tiff", dpi=300, height=4, width=18)

#check variable influence

reshaped_exposure_data <- exposure_data %>%
  st_drop_geometry() %>% 
  dplyr::select(crit_1_1, crit_1_2 ,exposure_scaled ) %>% 
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))


exposure_means<-ggplot(reshaped_exposure_data, aes(x = variable, y = mean)) +
  #geom_boxplot(position = position_dodge(0.8), alpha = 0.5, width = 0.6) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.8)) +
  theme_classic()  +
  labs(title="Exposure")+
  scale_y_continuous(limits=c(-0.2, 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

exposure_means


##################################################################################


#################################################################################
# COMPONENT 2. SENSITIVITY ######
#################################################################################


### 2_1 Composite index of employment and economic dependence on the small-scale fisheries sector ####

### 2_1_A Contribution to SSF marine fisheries landed value (% of global marine SSF landed value, IHH) ######
landed_value_data<-read.csv(
  here("data", "landed_value_ssf.csv"),
  stringsAsFactors = FALSE)

crit_2_1_A_data<- landed_value_data %>%  #this comes from "Global_SSF_LV" from IHH core datasets
  mutate(global_landed_value=sum(landed_value)) %>% 
  mutate(crit_2_1_A_raw=landed_value/global_landed_value) %>% 
  mutate(crit_2_1_A_log =log(crit_2_1_A_raw+1)/global_landed_value) %>% 
   mutate(crit_2_1_A= scales::rescale(crit_2_1_A_log) )


# plot
crit_2_1_A_histogram<-ggplot(crit_2_1_A_data) +
  geom_histogram( aes(x = crit_2_1_A))



### 2_1_B Contribution to SSF global marine fisheries employment (% of global marine SSF employment, IHH) ######
#Description: Percent contribution of number of fishers by country to global employment in fisheries
employment_data<- read.csv(  
  here("data", "employment_ssf.csv"),
  stringsAsFactors = FALSE)


crit_2_1_B_data<-employment_data %>% 
  dplyr::select(country,country_ISO_alpha3 ,   harvest_marine_SSF, harvest_marine_LSF) %>% 
  mutate(fishers= harvest_marine_LSF+ harvest_marine_SSF) %>% 
  mutate(global_fishers=sum(fishers)) %>% 
  mutate(crit_2_1_B_raw =fishers/global_fishers) %>% 
  arrange(desc(crit_2_1_B_raw)) %>% 
  filter(fishers!=0) %>% 
  mutate(crit_2_1_B_log=log(fishers+1)/global_fishers) %>% 
  mutate(crit_2_1_B= scales::rescale(crit_2_1_B_raw) )


# plot
crit_2_1_B_histogram<-ggplot(crit_2_1_B_data) +
  geom_histogram( aes(x = crit_2_1_B))


### 2_1_C Contribution of marine SSF employment in fisheries to total employment (% of country’s labor force, IHH)####
#Percent contribution of marine SSF fishers to total country labour force 

ssf_employment<-employment_data %>% 
  dplyr::select(country,country_ISO_alpha3 ,   harvest_marine_SSF)

ssf_employment$harvest_marine_SSF<-as.numeric(ssf_employment$harvest_marine_SSF)

labor_data<- read.csv( 
  here("data", "world_bank_employment_data.csv"),
  stringsAsFactors = FALSE)

global_employment<- labor_data %>% 
  mutate(employment_mean_raw=rowMeans(dplyr::select(., X2013: X2017) )) %>% 
  mutate(employment_log=log(employment_mean_raw)) %>% 
  filter(employment_log!=0) %>% 
  dplyr::select(country, employment_mean_raw, employment_log) 

crit_2_1_C_data<- ssf_employment %>% 
  left_join(global_employment, by="country") %>% 
  mutate(crit_2_1_C_raw = harvest_marine_SSF/employment_mean_raw)  %>%
  mutate(crit_2_1_C_log = log(harvest_marine_SSF)/employment_log)  %>%
  mutate(crit_2_1_C_raw = ifelse(is.infinite(crit_2_1_C_raw) & crit_2_1_C_raw < 0, 0, crit_2_1_C_raw)) %>% 
  mutate(crit_2_1_C_log = ifelse(is.infinite(crit_2_1_C_log) & crit_2_1_C_log < 0, 0, crit_2_1_C_log)) %>% 
   mutate(crit_2_1_C = scales::rescale(crit_2_1_C_log) ) %>% 
  drop_na() %>% 
  filter(crit_2_1_C!=0)

# plot
crit_2_1_C_histogram<-ggplot(crit_2_1_C_data) +
  geom_histogram( aes(x = crit_2_1_C))

shapiro.test(crit_2_1_C_data$crit_2_1_C)


### 2_1_D Contribution to marine subsistence employment (work for own consumption) (% of global subsistence workers, IHH) #####
#Percent contribution of number of marine subsistence fishers by country to global marine subsistence fishers

crit_2_1_D_data<-employment_data%>% 
  dplyr::select(country,country_ISO_alpha3 ,   Marine_harvest_WOC_SSF ) %>% 
  mutate(subsistence_fishers= Marine_harvest_WOC_SSF) %>% 
  drop_na(subsistence_fishers) %>% 
  filter(subsistence_fishers!=0) %>% 
  mutate(global_fishers=sum(subsistence_fishers)) %>% 
  mutate( crit_2_1_D_raw=subsistence_fishers/global_fishers) %>% 
  mutate(  crit_2_1_D_log =log(subsistence_fishers+1)/global_fishers) %>% 
  mutate(crit_2_1_D = scales::rescale( crit_2_1_D_log) ) %>% 
  drop_na()


# plot
crit_2_1_D_histogram<-ggplot(crit_2_1_D_data) +
  geom_histogram( aes(x = crit_2_1_D))

shapiro.test(crit_2_1_D_data$crit_2_1_D)


### Aggregate 2_1 variables ######

#Compile 2_1 employment data 
crit_2_1_data <-crit_2_1_A_data %>% 
  left_join(crit_2_1_B_data, by =c( "country_ISO_alpha3")) %>% 
  left_join(crit_2_1_C_data, by =c( "country_ISO_alpha3")) %>% 
  left_join(crit_2_1_D_data, by =c( "country_ISO_alpha3")) %>% 
  mutate(Subregion = countrycode(
    country_ISO_alpha3, 
    origin = "iso3c", 
    destination = "un.regionsub.name"), #assign region for imputing later
    Subregion = ifelse(country_ISO_alpha3 == "TWN", "Eastern Asia", Subregion)
  ) %>%
  mutate(Continent = countrycode(
    country_ISO_alpha3, 
    origin = "iso3c", 
    destination = "continent") #assign region for imputing later
  ) %>%
  dplyr::select(country_ISO_alpha3, Subregion,Continent,
                
                # crit_2_1_A_raw,
                # crit_2_1_B_raw,
                # crit_2_1_C_raw, 
                #  crit_2_1_D_raw) %>% 

                crit_2_1_A,
                crit_2_1_B,
                crit_2_1_C,
                crit_2_1_D ) %>%
  filter(rowSums(across(c(crit_2_1_A, crit_2_1_B, crit_2_1_C, crit_2_1_D), ~ is.na(.))) <= 2) #remove countries missing more than 2 data points



### 2_2 Composite index of coastal nutritional dependence on SSFs ######
### 2_2_A Contribution to marine SSF production within country (kg/  coastal cap /yr, IHH) [for coastal population within 100 km]####

coastal_population<-read.csv( 
  here("data", "coastal_population.csv"),
  stringsAsFactors = FALSE)

crit_2_2_A_data<- ssf_catch %>% 
  dplyr::left_join(coastal_population, by = c("country_ISO_alpha3") ) %>% 
  filter(coastal_population!=0) %>% 
  mutate(ssf_catch_kg = national_catch_final * 1000) %>% 
  mutate(crit_2_2_A_raw= ssf_catch_kg/coastal_population ) %>% 
  mutate(crit_2_2_A_log= log(ssf_catch_kg)/log(coastal_population )) %>% 
  mutate(crit_2_2_A = scales::rescale(crit_2_2_A_log) ) 

# plot
crit_2_2_A_histogram<-ggplot(crit_2_2_A_data) +
  geom_histogram( aes(x =crit_2_2_A))

#2.2_B Quality: Nutrient supply of catch (iron, zinc, calcium, vitamin A) for coastal residents [compiled domestic proportions that would meet 25% RDI for coastal population]

nutrition_data <-read.csv( 
  here("data", "coastal_population.csv"),
  stringsAsFactors = FALSE)


nutrition_data<-  read.csv( 
    here("data", "nutrition_data.csv"),
    stringsAsFactors = FALSE) %>% 
  filter(marine_inland_char=="marine") %>% 
  mutate(Continent = countrycode(
    sourcevar = country,
    origin = "iso3c",
    destination = "continent"
  )) %>% 
  dplyr::rename(country_ISO_alpha3 = country)


crit_2_2_B_data<- nutrition_data %>% 
  dplyr::left_join(coastal_population, by = c("country_ISO_alpha3") ) %>% 
  mutate(crit_2_2_B_raw= pop/coastal_population ) %>% 
  filter(crit_2_2_B_raw!="Inf") %>% 
  mutate(crit_2_2_B_log = log(crit_2_2_B_raw+1) ) %>%  
  mutate(crit_2_2_B = scales::rescale(crit_2_2_B_log) )


# plot
crit_2_2_B_histogram<-ggplot(crit_2_2_B_data) +
  geom_histogram( aes(x =crit_2_2_B_raw))

shapiro.test(crit_2_2_B_data$crit_2_2_B)



### 2_2_C Prevalence of inadequate micronutrient intake (PMII) for 14 micronutrients  ######
#Prevalence of inadequate micronutrient intake  (Beale et al 2017) 

crit_2_2_C_data<-read.csv( 
  here("data", "prevalence_micronutrient_deficiency.csv"), stringsAsFactors = FALSE) %>% 
                           rename(country_ISO_alpha3=ISO3) %>% 
  dplyr::select(country_ISO_alpha3 ,  PIMII.without.Fortification) %>% 
  rename(crit_2_2_C_raw =PIMII.without.Fortification) %>% 
  mutate(crit_2_2_C_log=log(crit_2_2_C_raw+1)) %>% 
  mutate(crit_2_2_C = scales::rescale(crit_2_2_C_log) ) 

# plot
crit_2_2_C_historgram <-ggplot(crit_2_2_C_data) +
  geom_histogram( aes(x = crit_2_2_C))

shapiro.test(crit_2_2_C_data$crit_2_2_C)


### Aggregate 2_2 variables ######
crit_2_2_data <-crit_2_2_A_data %>%
  left_join(crit_2_2_B_data, by =c( "country_ISO_alpha3")) %>%
  left_join(crit_2_2_C_data, by =c( "country_ISO_alpha3")) %>%
  mutate(Subregion = countrycode(
    country_ISO_alpha3, 
    origin = "iso3c", 
    destination = "un.regionsub.name"), #assign region for imputing later
    Subregion = ifelse(country_ISO_alpha3 == "TWN", "Eastern Asia", Subregion)
  ) %>%
  mutate(Continent = countrycode(
    country_ISO_alpha3, 
    origin = "iso3c", 
    destination = "continent") #assign region for imputing later
  ) %>%
  dplyr::select(country_ISO_alpha3, Subregion,Continent,
                crit_2_2_A_raw,
                crit_2_2_B_raw,
                crit_2_2_C_raw,

                crit_2_2_A,
                crit_2_2_B ,
                crit_2_2_C) %>%
  filter(
    !rowSums(is.na(.)) > 2)  # Keep countries with 2 or fewer missing values


### CALCULATE SENSITIVYTY INDEX (before imputation, see below). We don't impute here because we want to have all the other data for possible countries based on data availability for exposure and adaptive capacity #####
sensitivity_data<- crit_2_1_data %>% 
  left_join(crit_2_2_data, by =c( "country_ISO_alpha3", "Continent", "Subregion"))  %>% 
  dplyr::select(country_ISO_alpha3, Continent, Subregion,
                
                crit_2_1_A_raw,
                crit_2_1_B_raw,
                crit_2_1_C_raw,
                crit_2_1_D_raw,
                
                crit_2_2_A_raw,
                crit_2_2_B_raw,
                crit_2_2_C_raw,
                
                crit_2_1_A,
                crit_2_1_B,
                crit_2_1_C,
                crit_2_1_D,
                
                crit_2_2_A,
                crit_2_2_B,
                crit_2_2_C
                )


# reshaped_data <- sensitivity_data %>%
#   gather(key = "variable", value = "value", -country_ISO_alpha3) %>%
#   group_by(variable) %>%
#   summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
# 
# sensitivity_means<- ggplot(reshaped_data, aes(x = variable, y = mean)) +
#   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.8)) +
#   theme_classic() +
#   labs(title="Sensitivity")+
#   scale_y_continuous(limits=c(-.1, 1))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# sensitivity_means #without imputation



###############################################################################

#################################################################################
# COMPONENT 3. ADAPTIVE CAPACITY 
#################################################################################

### 3_1 HDI ####

hdi<-read.csv( 
  here("data", "hdi_time_series.csv"),
  stringsAsFactors = FALSE) %>% 
  dplyr::select(country_ISO_alpha3, country,
                hdi_2013, hdi_2014, hdi_2015, hdi_2017, hdi_2017)

crit_3_1_data<-hdi %>% 
  dplyr::mutate(crit_3_1_raw= rowMeans(dplyr::select(., hdi_2013:hdi_2017) ))  %>%
  filter(!is.na(crit_3_1_raw)) %>% 
  mutate(crit_3_1= scales::rescale(crit_3_1_raw)) #Crit 3_1 does not need to be normalized

crit_3_1_histogram<- ggplot(crit_3_1_data) +
  geom_histogram(aes(x = crit_3_1_raw), na.rm = TRUE)

shapiro.test(crit_3_1_data$crit_3_1)


### 3_2 Development and governance ########
#Political Stability and Absence of Violence Estimate, Government Effectiveness Estimate, Regulatory Quality Estimate, Rule of law , Voice and accountability

governance<-read.csv( 
  here("data", "governance_data.csv"),
  stringsAsFactors = FALSE) %>% 
  filter(country_ISO_alpha3!="") %>% 
  dplyr::select(country_ISO_alpha3, corruption, gov_effectiveness, political_stability, regulatory_quality, rule_of_law, voice_accountability) %>% 
  mutate_at(vars(corruption, gov_effectiveness, political_stability, regulatory_quality, rule_of_law, voice_accountability),
            ~ ifelse(. == "ND", NA, as.numeric(.))  ) %>% 
  rename(crit_3_2_A_raw=  corruption,
         crit_3_2_B_raw=gov_effectiveness,
           crit_3_2_C_raw= political_stability,
           crit_3_2_D_raw= regulatory_quality,
           crit_3_2_E_raw= rule_of_law,
           crit_3_2_F_raw= voice_accountability) %>% 
    mutate(crit_3_2_A= scales::rescale(crit_3_2_A_raw ) ,
           crit_3_2_B= scales::rescale(crit_3_2_B_raw ),
           crit_3_2_C= scales::rescale(crit_3_2_C_raw ),
           crit_3_2_D= scales::rescale(crit_3_2_D_raw ),
           crit_3_2_E= scales::rescale(crit_3_2_E_raw ),
           crit_3_2_F= scales::rescale(crit_3_2_F_raw )
           
           )
  
governance<-as.data.frame(governance)

crit_3_2_data<- governance %>% 
  dplyr::mutate(crit_3_2_raw = rowMeans(dplyr::select(., crit_3_2_A_raw:crit_3_2_F_raw), na.rm = TRUE ))  %>% 
  dplyr::mutate(crit_3_2 = rowMeans(dplyr::select(., crit_3_2_A:crit_3_2_F), na.rm = TRUE ))  

#plot
crit_3_2_histogram<-ggplot(crit_3_2_data) +
  geom_histogram(aes(x = crit_3_2_raw), na.rm = TRUE)

shapiro.test(crit_3_2_data$crit_3_2)



### CALCULATE ADAPTIVE CAPACITY #####
adaptive_capacity_data<- crit_3_1_data %>% 
  left_join(crit_3_2_data, by =c( "country_ISO_alpha3"))  %>% 
  mutate(adaptive_capacity_raw = rowSums(dplyr::select(., 
                                                    crit_3_1_raw,
                                                    crit_3_2_raw
  ), na.rm = TRUE)) %>% 
  mutate(adaptive_capacity = rowSums(dplyr::select(., 
                                                       crit_3_1,
                                                       crit_3_2
  ), na.rm = TRUE)) %>% 
  mutate(adaptive_capacity_log=log(adaptive_capacity+1)) %>% 
  mutate(adaptive_capacity=scales::rescale(adaptive_capacity_log)) %>% 
  dplyr::select(country_ISO_alpha3,adaptive_capacity,adaptive_capacity_raw, adaptive_capacity_log, 
                crit_3_1_raw, crit_3_2_raw,crit_3_2_A_raw, crit_3_2_B_raw,crit_3_2_C_raw, crit_3_2_D_raw,crit_3_2_E_raw, crit_3_2_F_raw,
                crit_3_1, crit_3_2,crit_3_2_A, crit_3_2_B,crit_3_2_C, crit_3_2_D,crit_3_2_E, crit_3_2_F)

#get inverse of adaptive capacity
 max_normal_adaptive_capacity <- max(adaptive_capacity_data$adaptive_capacity)
 max_normal_adaptive_capacity_raw <- max(adaptive_capacity_data$adaptive_capacity_raw)
 
# 
adaptive_capacity_data <- adaptive_capacity_data %>%
   mutate(adaptive_capacity_inverse  = max_normal_adaptive_capacity - adaptive_capacity ) %>% 
  mutate(adaptive_capacity_inverse_raw  = max_normal_adaptive_capacity_raw - adaptive_capacity_raw )

ggplot(adaptive_capacity_data) +
  geom_histogram(aes(x =adaptive_capacity_inverse_raw), na.rm = TRUE)


shapiro.test(adaptive_capacity_data$adaptive_capacity)

#make sure to use adaptive_capacity_inverse in ultimate calculations


reshaped_adaptive_capacity_data <- adaptive_capacity_data %>%
  gather(key = "variable", value = "value", -country_ISO_alpha3) %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))


adaptive_capacity_means<- ggplot(reshaped_adaptive_capacity_data, aes(x = variable, y = mean)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.8)) +
  theme_classic() +
  labs(title="Adaptive Capacity")+
  scale_y_continuous(limits=c(-0.2, 1))+
 theme(axis.text.x = element_text(angle = 45, hjust = 1))



#exposure_means + sensitivity_means + adaptive_capacity_means

#ggsave("subindex_means.tiff", dpi=300, width=10, height=5)


#################################################################################

#################################################################################
# IMPUTE MISSING VALUES
#################################################################################

#sensitivity values are missing for some countries, so going to recalculate with continental averages. 

# Calculate the continent-wise averages for crit_2_1_sum
crit_2_1_region_avg <- crit_2_1_data %>%
  group_by(Continent) %>%
  summarize( 
    mean_2_1_A = mean(crit_2_1_A, na.rm = TRUE),
    mean_2_1_B = mean(crit_2_1_B, na.rm = TRUE),
    mean_2_1_C = mean(crit_2_1_C, na.rm = TRUE),
    mean_2_1_D = mean(crit_2_1_D, na.rm = TRUE)
  ) %>%
     mutate( mean_2_1_D = ifelse(Continent == "Europe", 0, mean_2_1_D)) #no subsistence data for Europe, we assume near 0.


# Merge the continent averages back into the data and impute missing values
crit_2_1_data_complete <- crit_2_1_data  %>%
  left_join(crit_2_1_region_avg , by = c("Continent")) %>%
   mutate(
    crit_2_1_A = ifelse(is.na(crit_2_1_A), mean_2_1_A, crit_2_1_A),
    crit_2_1_B = ifelse(is.na(crit_2_1_B), mean_2_1_B, crit_2_1_B),
    crit_2_1_C = ifelse(is.na(crit_2_1_C), mean_2_1_C, crit_2_1_C),
    crit_2_1_D = ifelse(is.na(crit_2_1_D), mean_2_1_D, crit_2_1_D)
  )


crit_2_1_data<- crit_2_1_data_complete %>%
  mutate(crit_2_1 = scales::rescale(rowMeans(dplyr::select(.,
                                                           crit_2_1_A,
                                                           crit_2_1_B,
                                                           crit_2_1_C,
                                                           crit_2_1_D    ), na.rm = TRUE))) %>%
  filter(crit_2_1!="-Inf")


# Calculate the continent-wise averages for crit_2_1
crit_2_2_region_avg <- crit_2_2_data%>%
  group_by(Continent) %>%
  summarize(
    mean_2_2_A = mean(crit_2_2_A, na.rm = TRUE),
    mean_2_2_B = mean(crit_2_2_B, na.rm = TRUE),
   mean_2_2_C = mean(crit_2_2_C, na.rm = TRUE)
  )

# # Merge the continent averages back into the data and impute missing values
crit_2_2_data_complete <- crit_2_2_data %>%
  left_join(crit_2_2_region_avg, by = "Continent") %>%
  mutate(
    crit_2_2_A = ifelse(is.na(crit_2_2_A), mean_2_2_A, crit_2_2_A),
    crit_2_2_B = ifelse(is.na(crit_2_2_B), mean_2_2_B, crit_2_2_B),
    crit_2_2_C = ifelse(is.na(crit_2_2_C), mean_2_2_C, crit_2_2_C)
  )

#
crit_2_2_data <-crit_2_2_data_complete %>%
  mutate(crit_2_2 = rowMeans(dplyr::select(.,
                                                           crit_2_2_A,
                                                           crit_2_2_B ,
                                                           crit_2_2_C), na.rm = TRUE)) %>%
  mutate( crit_2_2= scales::rescale(crit_2_2))

sensitivity_data <- crit_2_1_data %>%
  inner_join(crit_2_2_data,by = c("country_ISO_alpha3", "Continent", "Subregion")) %>%
  mutate(
    sensitivity = rowMeans(dplyr::select(., crit_2_1, crit_2_2), na.rm = TRUE)
  )




######## MERGE 3 INDICES  ###########################
#put all criteria together in one df
exposure_data_df<- exposure_data_df %>%  rename(country_ISO_alpha3= Alpha.3.code)


all_components <- exposure_data_df %>% 
  left_join(sensitivity_data, by =c( "country_ISO_alpha3")) %>% 
  left_join(adaptive_capacity_data, by =c( "country_ISO_alpha3")) %>% 
  mutate(Continent = countrycode(
    country_ISO_alpha3, 
    origin = "iso3c", 
    destination = "continent"
  )) %>% 
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%  #remove a couple NaNs and replace with NA
  mutate(
         sensitivity_scaled=scales::rescale(sensitivity),
         adaptive_capacity_scaled=scales::rescale(adaptive_capacity_raw),
         adaptive_capacity_inverse_scaled=scales::rescale(adaptive_capacity_inverse))
  
#exclude country without all three indices
all_components <- all_components[complete.cases(all_components[c('sensitivity_scaled', 'adaptive_capacity_inverse_scaled', 'exposure_scaled')]), ]

#check simple methods of producing the SIFI Index
df <- all_components %>%
  group_by(country_ISO_alpha3) %>%
  mutate(   overall_index_sum =( exposure_scaled + sensitivity_scaled + adaptive_capacity_inverse_scaled), 
            
            overall_index_prod = (prod(c(exposure_scaled , sensitivity_scaled + adaptive_capacity_inverse_scaled))),
                                        
            overall_index_mean = (mean(c(exposure_scaled , sensitivity_scaled + adaptive_capacity_inverse_scaled)))) %>%

  ungroup() %>%  # Ungroup the data
  mutate(
    index_sum_scaled = scales::rescale(log(overall_index_sum+1), to = c(0, 1)),
    
    index_prod_scaled = scales::rescale(log(overall_index_prod+1), to = c(0, 1)),
   
    index_mean_scaled = scales::rescale(log(overall_index_mean+1), to = c(0, 1))
  ) 



#PLOT SUBINDEX SCALED DISTRIBUTIONS ######


# Create individual plots for each code line
plotA <- ggplot(df) +
  geom_histogram(aes(x = adaptive_capacity_scaled), na.rm = TRUE, bins = 30) +
  #color_scale +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Adaptive capacity")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

plotB <- ggplot(df) +
  geom_histogram(aes(x = exposure_scaled), na.rm = TRUE, bins = 30) +
#  color_scale +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Exposure")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

plotC <- ggplot(df) +
  geom_histogram(aes(x = sensitivity_scaled), na.rm = TRUE, bins = 30) +
  #color_scale +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Sensitivity")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

plotD <- ggplot(df) +
  geom_histogram(aes(x =index_mean_scaled), na.rm = TRUE, bins = 30) +
#  color_scale +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Vulnerability")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

A_plot <- plot_grid(plotB, plotC, plotA, ncol = 3)
A_plot
ggsave("histograms.tiff", dpi=300, width=15, height=3)

plot1 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(country_ISO_alpha3, adaptive_capacity_scaled), 
               y = adaptive_capacity_scaled, 
               fill = adaptive_capacity_scaled)) +
  color_scale +
  theme_classic() +
  labs(y="Adaptive capacity", x="Country")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot2 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(country_ISO_alpha3, exposure_scaled), 
               y = exposure_scaled, 
               fill = exposure_scaled)) +
  color_scale +
  coord_flip()+
  labs(y="Exposure", x="Country")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot3 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(country_ISO_alpha3, sensitivity_scaled), 
               y = sensitivity_scaled, 
               fill = sensitivity_scaled)) +
  color_scale +
  labs(y="Sensitivity", x="Country")+
  theme_classic() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot4 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(country_ISO_alpha3, -index_prod_scaled), 
               y = index_prod_scaled, 
               fill = index_prod_scaled)) +
  color_scale +
  theme_classic() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

# Arrange the individual plots in one column
B_plot <- plot_grid(plot1, plot2, plot3,  ncol = 3)

# Display the combined plot
A_plot / B_plot +plot_layout(heights=c(1,4))



ggsave("subindex_distribution.tiff", dpi=300, width=5, height=15)



# Calculate correlation between mean, sum, and product indices
cor_mean_sum <- cor(df$index_mean_scaled, df$index_sum_scaled, use = "complete.obs")
cor_mean_prod <- cor(df$index_mean_scaled, df$index_prod_scaled, use = "complete.obs")
cor_sum_prod <- cor(df$index_sum_scaled, df$index_prod_scaled, use = "complete.obs")

# Print the correlation coefficients
cat("Correlation between Mean and Sum indices:", cor_mean_sum, "\n")
cat("Correlation between Mean and Product indices:", cor_mean_prod, "\n")
cat("Correlation between Sum and Product indices:", cor_sum_prod, "\n")

#### sum stats and sensitivity ######
df_sum_stats<-df %>%
  ungroup() %>% 
  summarise(mean_exposure= mean(exposure_scaled),
             mean_sensitivity= mean(sensitivity_scaled, na.rm = TRUE),
            mean_adaptive_capacity= mean(adaptive_capacity_inverse_scaled, na.rm = TRUE),
           mean_prod_index= mean(index_prod_scaled , na.rm = TRUE),
           mean_sum_index= mean(index_sum_scaled , na.rm = TRUE))

df_sum_stats

# Calculate quartiles for normalized_overall_index
quartiles <- quantile(df$index_prod_scaled, probs = c(0, 0.25, 0.5, 0.75, 1),  na.rm=TRUE)

# Determine the threshold value for the top quartile
threshold <- quartiles[4]  # 75th percentile

# Filter countries in the top quartile
top_quartile <- df[df$index_prod_scaled >= threshold, ]

# Find the minimum index score in the top quartile
min_index_score <- min(top_quartile$index_prod_scaled )

# List of focal countries to highlight, just to explore how they sit within the data
highlight_countries <- c("BRA", "GMB", "GAB", "ARG", "ESP", "CHL", "GNQ", "PER", "VCT")

df_without_missing <- df[!is.na(df$index_prod_scaled), ]

# Create the scatter plot with quartiles and lines
ggplot(df, aes(x = fct_reorder(country_ISO_alpha3,index_prod_scaled),y = index_prod_scaled )) +
  geom_rect(
    xmin = -Inf, xmax = Inf, ymin = threshold, ymax = 1,
    fill = "lightgray", aes(alpha = 0.9 ) # Shaded rectangle for top quartile
  ) + 
  geom_point(aes(color = country_ISO_alpha3 %in% highlight_countries)) +  # Highlight countries by color
  geom_hline(yintercept = quartiles, color = "blue", linetype = "dashed") +  # Add horizontal lines for quartiles
  geom_text(   aes(label = ifelse(country_ISO_alpha3 %in% highlight_countries, country_ISO_alpha3, "")),
    vjust = -0.5,  # Adjust vertical position of labels
    show.legend = FALSE   ) +# Hide the legend for the labels 
  labs(   x = "Country",  y = "Normalized Overall Index Rank", title = "Countries' Overall Index Rank vs. Quartiles"  ) +
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels for better readability
  theme(
    text = element_text(size = 24),
    axis.text.x = element_text(size = 10) 
  )
#ggsave("quartile_country_locations.tiff", dpi=300, height=8, width=12)

#### SENSITIVITY ANALYSES ###############################
##PCA for sensitivity 
# Select the columns you want to include in PCA (excluding non-numeric columns)

selected_columns <- df%>%
  dplyr::select(exposure_scaled, sensitivity_scaled, adaptive_capacity_inverse_scaled)

# Perform PCA
pca_result <- prcomp(selected_columns, scale. = TRUE)

# Explore the PCA results
summary(pca_result)

# Access the loadings, standard deviations, and scores
loadings(pca_result)     # Loadings of each variable on the principal components
pca_result$sdev         # Standard deviations of the principal components
pca_result$x            # Scores (values of each observation on the principal components)


# Define names for the principal components based on subindex names
pc_names <- c("Exposure", "Sensitivity", "Adaptive Capacity")  # Add more names as needed
custom_colors <- c("Exposure" = "#2CA02C", "Sensitivity" = "cornflowerblue","Adaptive Capacity"= "salmon" )

# Scree plot to visualize the explained variance by each principal component
scree_data <- data.frame(
  PC = pc_names,  # Use the names vector as the x-axis labels
  VarianceExplained = (pca_result$sdev^2) / sum(pca_result$sdev^2),
  Color = pc_names  # Add a column for matching colors
)

# Map colors to the Color column based on PC names
scree_data$Color <- custom_colors[scree_data$Color]

# Create the scree plot with named principal components and custom colors
scree_plot <- ggplot(scree_data, aes(x = PC, y = VarianceExplained,  fill = Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Principal Component", y = "Variance Explained (%)") +
  scale_fill_identity() +  # Use identity scale for custom colors
  theme_minimal()


# Biplot to visualize loadings and scores
biplot_data <- as.data.frame(pca_result$rotation[, 1:2])
biplot_data$Variable <- rownames(pca_result$rotation[, 1:2])

# Create a subset of pca_result$x with the same number of rows as biplot_data
num_rows <- nrow(biplot_data)
pca_scores_subset <- pca_result$x[1:num_rows, ]

# Add PC1 and PC2 to biplot_data
biplot_data$PC1 <- pca_scores_subset[, 1]
biplot_data$PC2 <- pca_scores_subset[, 2]


# Color the points in the biplot based on the PC values
biplot_plot<- ggplot(biplot_data, aes(x = PC1, y = PC2, color = Variable)) +
  geom_segment(data = biplot_data,
               aes(x = 0, y = 0, xend = PC1, yend = PC2, color = Variable),
               arrow = arrow(type = "closed", length = unit(0.02, "npc")),
               linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text_repel(
    aes(label = Variable, color=Variable),
    nudge_x = 0.1, nudge_y = 0.1
  ) +
  labs(x = "PC1", y = "PC2") +
  theme_minimal()+
  theme(legend.position="none")

# Display the biplot and scree plot side by side
biplot_plot+ scree_plot

#ggsave("PCA_plotsa.tiff", dpi=300, width=10, height=7)



# Create an empty data frame to store sensitivity results
sensitivity_results <- data.frame(Subindex = character(0), Correlation = numeric(0))

# Loop through each subindex
subindices <- c("exposure_scaled", "sensitivity_scaled", "adaptive_capacity_inverse_scaled")

for (subindex in subindices) {
  correlation_result <- cor(df_without_missing[[subindex]], df_without_missing$index_mean_scaled)
  sensitivity_results <- rbind(sensitivity_results, data.frame(Subindex = subindex, Correlation = correlation_result))
}



sensitivity_plot <- ggplot(sensitivity_results, aes(x = Subindex, y = Correlation, fill = Subindex)) +
  geom_bar(stat = "identity") +
  labs(x = "Subindex", y = "Correlation with Overall Index") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_minimal()

# Display the sensitivity plot
print(sensitivity_plot)

# Compute the correlation matrix for all variables, including subindices
correlation_matrix <- cor(df_without_missing[, c("exposure_scaled", "sensitivity_scaled", "adaptive_capacity_inverse_scaled", "index_mean_scaled")])

# Customize the color scale for the heatmap
colors <- colorRampPalette(rev(brewer.pal(6, "RdYlBu")))(100)

# Convert the correlation matrix to a data frame for ggplot
correlation_data <- as.data.frame(correlation_matrix)

# Rename rows and columns of the correlation matrix
rownames(correlation_data) <- c("Exposure", "Sensitivity", "Adaptive_Capacity", "Vulnerability")
colnames(correlation_data) <- c("Exposure", "Sensitivity", "Adaptive_Capacity", "Vulnerability")

library(reshape2)
# Melt the correlation matrix for plotting
melted_data <- melt(correlation_data)
melted_data$Var1 <- rownames(correlation_data)  # Add Var1 column with correct names
melted_data$Var2 <- rownames(correlation_data)[melted_data$variable]  # Add Var2 column with correct names

# Define a color palette ranging from red to blue
colors <- colorRampPalette(c("red", "white", "blue"))(100)

# Create the heatmap
ggplot(melted_data, aes(x = Var1, y = Var2, fill = value)) +
         geom_tile()+
scale_fill_gradientn(
  colors = colors,  # Use the defined color palette
  limits = c(-1, 1),
  breaks = seq(-1, 1, 1),
  labels = scales::percent_format(scale = 1)
) +  
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(text=element_text(size=14))

ggsave("correlation_plot_index.png", dpi=300, height=6, width=7)

##CREATE MAPS ######

world_shp_iso <- world_shp %>%
  mutate(Alpha.3.code = countrycode(sourcevar = ID, origin = "country.name", destination = "iso3c"))

df<-df %>%  rename(Alpha.3.code =country_ISO_alpha3)
df_spatial <- merge(world_shp_iso, df,by="Alpha.3.code" )

####

# Create a bar chart of the top 20 ranking exposure countries
top_20_vulnerability <- df_spatial %>% 
  arrange(desc(index_mean_scaled)) %>% 
  slice(1:20)

bar_A <- ggplot(top_20_vulnerability, aes(x = reorder(Alpha.3.code, index_prod_scaled), y = index_prod_scaled, fill=index_prod_scaled)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand=c(0,0))+
  coord_flip() +
  labs(x = NULL, y = "Vulnerability") +
  color_scale +  # Use the same scale as in the map
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

bar_A


###### EXPOSURE

exposure_map<-
  ggplot() +
  geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
  geom_sf(data = df_spatial , aes(fill = exposure_scaled), color = "darkgray") +
  color_scale + 
  labs( fill = "Exposure") +
  theme_bw()+
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

exposure_map


# Create a bar chart of the top 20 ranking exposure countries
top_20_exposure <- df_spatial %>% 
  arrange(desc(exposure_scaled)) %>% 
  slice(1:20)

bar_exposure <- ggplot(top_20_exposure, aes(x = reorder(Alpha.3.code, exposure_scaled), y = exposure_scaled, fill=exposure_scaled)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand=c(0,0))+
  coord_flip() +
  labs(x = NULL, y = "Exposure") +
  color_scale +  # Use the same scale as in the map
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

bar_exposure


sensitivity_map<-
  ggplot() +
  geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
  geom_sf(data=df_spatial, aes(fill =sensitivity_scaled), color = "darkgray") +
  color_scale+
  labs( fill = "Sensitivity") +
  theme_bw()+
  theme(legend.position = "top", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

sensitivity_map


# Create a bar chart of the top 20 ranking countries
top_20_sensitivity <- df_spatial %>% 
  arrange(desc(sensitivity_scaled)) %>% 
  slice(1:20)

sensitivity_bar <- ggplot(top_20_sensitivity, aes(x = reorder(Alpha.3.code, sensitivity_scaled), y = sensitivity_scaled, fill=sensitivity_scaled)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand=c(0,0))+
  coord_flip() +
  labs(x = NULL, y = "Sensitivity") +
  color_scale +  # Use the same scale as in the map
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )
  
sensitivity_bar 


adaptive_capacity_map<- ggplot() +
  geom_sf(data = world_shp , fill = "lightgray", color = "darkgray") +
  geom_sf(data=df_spatial, aes(fill =adaptive_capacity_scaled), color = "darkgray") +
  scale_fill_viridis_c(option = "magma", direction = 1, na.value = "gray", limits = color_range)+
  labs( fill = "Adaptive capacity") +
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

adaptive_capacity_map

# Create a bar chart of the top 20 ranking exposure countries
top_20_adaptive_capacity <- df_spatial %>% 
  arrange((adaptive_capacity_inverse_scaled)) %>% 
  slice(1:20)

adaptive_capacity_bar <- ggplot(top_20_adaptive_capacity, aes(x = reorder(Alpha.3.code, adaptive_capacity_scaled), y = adaptive_capacity_scaled, fill=adaptive_capacity_scaled)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand=c(0,0))+
  coord_flip() +
  labs(x = NULL, y = "Adaptive Capacity") +
  scale_fill_viridis_c(option = "magma", direction = 1, na.value = "gray", limits = color_range)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  theme(
    text = element_text(size = 16),  
    axis.text = element_text(size = 14) )

adaptive_capacity_bar 







#OTHER PLOTS #######
#exploratory analyses of relationships between components

# Set a threshold value for displaying labels
threshold_value <- 0.8  # Adjust this value to your preference

# Create bubble plot to show relationship between exposure and s, ac
plot_data_lm<- df %>% 
 # filter(Alpha.3.code!="CHN") %>% 
  mutate(mean_sensitivity_adaptive = rowMeans(cbind(sensitivity_scaled, adaptive_capacity_inverse_scaled)))

linear_model <- lm(exposure_scaled ~ mean_sensitivity_adaptive, data = plot_data_lm)
summary(linear_model)

# Extract p-value for the slope coefficient
p_value_slope <- summary(linear_model)$coefficients["mean_sensitivity_adaptive", "Pr(>|t|)"]


mean_S_AC<-plot_data_lm%>%
  ggplot(aes(y = exposure_scaled, x = mean_sensitivity_adaptive)) +
  geom_point(aes(alpha=0.8), size=4) +
  labs(y = "Exposure", x  = "Mean(Adaptive capacity, Sensitivity)" , title="Exposure - Mean(S, AC)") +
  theme_classic()+
  geom_smooth(method="lm", color="navy", fill = "navy")+
  theme(legend.position="none")+
  theme(text=element_text(size=16))+
  scale_y_continuous(limits=c(0,1))+
  annotate("text", x = 0.3, y = 0.9, 
           label = paste("Slope: ", round(coef(linear_model)[2], 4), "\nIntercept: ", round(coef(linear_model)[1], 4), "\np-value: ", format(p_value_slope, scientific = TRUE)), 
       color = "black")
mean_S_AC


#senstivity alone 
linear_model_S <- lm(exposure_scaled ~ sensitivity_scaled, data =  df)
summary(linear_model_S)

# Extract p-value for the slope coefficient
p_value_slope_S <- summary(linear_model_S)$coefficients["sensitivity_scaled", "Pr(>|t|)"]

sensitivity_plot<- df %>%
  ggplot(aes(y = exposure_scaled, x = sensitivity_scaled)) +
  geom_point(aes(alpha=0.8), size=4) +
  labs(y = "Exposure", x  = "Sensitivity", title="Exposure - Sensitivity") +
  theme_classic()+
  geom_smooth(method="lm", color="navy", fill = "navy")+
  theme(legend.position="none")+
  theme(text=element_text(size=16))+
  annotate("text", x = 0.3, y = 0.9, 
           label = paste("Slope: ", round(coef(linear_model_S)[2], 4), "\nIntercept: ", round(coef(linear_model_S)[1], 4), "\np-value: ", format(p_value_slope_S, scientific = TRUE)), 
           color = "black")

sensitivity_plot

linear_model_AC <- lm(exposure_scaled ~ adaptive_capacity_inverse_scaled, data =  df)
summary(linear_model_AC)

# Extract p-value for the slope coefficient
p_value_slope_AC <- summary(linear_model_AC)$coefficients["adaptive_capacity_inverse_scaled", "Pr(>|t|)"]

AC_plot<-df %>%
  ggplot(aes(y = exposure_scaled, x = adaptive_capacity_inverse_scaled)) +
  geom_point(aes(alpha=0.8), size=4) +
  labs(y = "Exposure", x  = "Adaptive Capacity", title="Exposure - Adaptive Capacity") +
  theme_classic()+
  geom_smooth(method="lm", color="navy", fill = "navy")+
  theme(legend.position="none")+
 # geom_text(aes(label = Alpha.3.code), vjust = -0.5, hjust = 0.5, size = 3) + 
  theme(text=element_text(size=16))+
  annotate("text", x = 0.3, y = 0.9, 
           label = paste("Slope: ", round(coef(linear_model_AC)[2], 4), "\nIntercept: ", round(coef(linear_model_AC)[1], 4), "\np-value: ", format(p_value_slope_AC, scientific = TRUE)), 
           color = "black")
AC_plot

mean_S_AC + sensitivity_plot + AC_plot

