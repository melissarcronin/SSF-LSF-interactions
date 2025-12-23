###############################################################################
# 01 Data cleaning and variable standardization
# 
# Description: 
#   This script cleans and standardizes all SIFI Index data and constructs the SIFI Index.
#     It is part of the SSF-LSF Interactions project.
#
# Author: Melissa Cronin
# Created: 2024-06-14
# Last Updated: 2025-12-23
#
# Run Order:
#   This is script 01 in the workflow:
#     01_data_cleaning_senstivity_analyses.R  →  02_Construct_SIFI_Index_and_visualization.R → 03_Bright_spot_analyses
###############################################################################

# Load Required Libraries
###############################################################################

# --- 1. Initialize Workspace -------------------------------------------------
rm(list = ls())  

# --- 2. Install and Load Packages -------------------------------------------
required_packages <- c("here",
  # Data manipulation
  "dplyr", "tidyr", "forcats", "countrycode","stringr", "countrycode","reshape2","here",
  
  # Visualization
  "ggplot2", "viridis", "ggrepel", "scico", "patchwork", "cowplot","RColorBrewer",
  "rnaturalearth",
  # Mapping
  "sf", "rnaturalearth", "rnaturalearthdata", "cartogram", 
  "raster",  "maps",
  
  # Tables and summaries
  "table1", "ggpubr",
  
  # Utility
  "scales", "gridExtra", "grid"
)

# Install packages
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
##### First, visual representation of nearshore area of analysis #######
#Visualize the square km of area within 25km for every country, using a buffer created in GIS
##this is just for visuals! We use the GFW-calculated area for 25km from shore to standardize between crit_1_1 and crit_1_2

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

#manually fix country codes for territories#####
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

# Create a data frame with country names and ISO codes ######
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

#this is just for visuals. We use the GFW-calculated area for 25km from shore to standardize between crit_1_1 and crit_1_2
#write.csv(coastal_buffer_df, "coastal_buffer_iso.csv")

ggplot() +
  geom_sf(data = world_shp, fill = "lightgray", color = "darkgray") +
  geom_sf(data = coastal_buffer_final, aes(color = sum_area)) +
  scale_color_viridis() +
  labs( color="Nearshore area (km^2)") +
  theme_classic()


###############################################################################

# COMPONENT 1. EXPOSURE ########
###############################################################################

### 1_1 Construct composite index of intensity of nearshore large-scale fishing activity ####
#pre-emplively filter all inpur data to only include data for which we have exposure data and MOST sensitivity data (see methods) this ensures that data are scaled from 0 to 1 with all countries that should be included
country_list <- read.csv(here("data", "raw", "country_list_from_all_components.csv"))$Alpha.3.code

#manually add data for a few countries that are reported separately. these decisions are made based on GFW precedent.
eez_manual_iso <- tibble::tribble(
  ~GEONAME,                                       ~Alpha.3.code,
  "United States Exclusive Economic Zone (Alaska)",     "USA",
  "United States Exclusive Economic Zone (Hawaii)",     "USA",
  "Indian Exclusive Economic Zone (Andaman and Nicobar Islands)", "IND",
  "Ecuadorian Exclusive Economic Zone (Galapagos)",     "ECU",
  "Spanish Exclusive Economic Zone (Canary Islands)",   "ESP",
  "Overlapping claim Kuril Islands: Japan / Russia",     "JPN",   # Japan controls populated islands
  "Portuguese Exclusive Economic Zone (Azores)",        "PRT",
  "Portuguese Exclusive Economic Zone (Madeira)",       "PRT",
  "Overlapping claim Senkaku Islands: Japan / China / Taiwan", "TWN", # administered by Taiwan in GFW coding
  "Overlapping claim Navassa Island: USA / Haiti / Jamaica",  "HTI",
  "Overlapping claim Ile Tromelin: Reunion / Madagascar / Mauritus", "FRA",
  "Oecussi Ambeno Exclusive Economic Zone",              "TLS",
  "Colombian Exclusive Economic Zone (Serranilla)",      "COL",
  "Overlapping claim Doumeira Islands: Djibouti / Eritrea", "ERI",
  "Overlapping claim Ceuta: Spain / Morocco",            "ESP",
  "Overlapping claim Chafarinas Islands: Spain / Morocco","ESP",
  "Overlapping claim Melilla: Spain / Morocco",          "ESP",
  "Overlapping claim Alhucemas Islands: Spain / Morocco","ESP",
  "Overlapping claim Peñón de Vélez de la Gomera: Spain","ESP"
)

gfw_raw <- read.csv(here("data", "raw", "activity_close_to_shore.csv"))

gfw_raw <- gfw_raw %>%
  left_join(eez_manual_iso, by = "GEONAME") %>%   # <-- join manual mapping
  mutate(
    ISO_TER1 = ifelse(ISO_TER1 == "" & !is.na(Alpha.3.code), Alpha.3.code, ISO_TER1)
  ) %>%
  dplyr::select(-Alpha.3.code) %>% 
  filter(ISO_TER1 != "")


#1. Read CSV
gfw_data <- gfw_raw %>% 
  filter(dist_km == 25) %>%
  rename(Alpha.3.code = ISO_TER1,
         nonbroadcasting_fishing =dark_fishing) %>%
  filter(Alpha.3.code %in% country_list) %>% 
  dplyr::select(
   Alpha.3.code,
    area_km2,        
    ais_fishing,
   nonbroadcasting_fishing ,
   fishing_per_km2,
  ) %>%
  group_by(Alpha.3.code) %>%  # area is constant per country
  summarise(
    nearshore_area=sum(area_km2),
    ais_fishing   = sum(ais_fishing,   na.rm = TRUE),
    nonbroadcasting_fishing  = sum(nonbroadcasting_fishing,  na.rm = TRUE),
    fishing_ratio=sum(fishing_per_km2),
    .groups = "drop"
  ) %>%
  mutate(  # Normalize by coastal area
    ais_density  = ais_fishing  / nearshore_area,
    nonbroadcasting_density= nonbroadcasting_fishing / nearshore_area
  )

# Calculate proportion of nearshore effort that is AIS matched vs not.
total_ais  <- sum(gfw_data$ais_fishing,  na.rm = TRUE)
total_nonbroadcasting <- sum(gfw_data$nonbroadcasting_fishing, na.rm = TRUE)
total_all          <- total_ais + total_nonbroadcasting

ais_share_global  <- total_ais  / total_all
nonbroadcasting_share_global <- total_nonbroadcasting / total_all

cat("Global AIS share:  ", round(ais_share_global  * 100, 2), "%\n")
cat("Global NONBROADCASTING share: ", round(nonbroadcasting_share_global * 100, 2), "%\n")

crit_1_1_data <- gfw_data %>%
  mutate(country_name = countrycode(Alpha.3.code, "iso3c", "country.name")) %>%
  mutate(
    crit_1_1_A_raw =log(ais_density+1 ),
    crit_1_1_B_raw = log(nonbroadcasting_density +1 ) ) %>%
  mutate(
    crit_1_1_A = scales::rescale(crit_1_1_A_raw, to = c(0, 1)),
    crit_1_1_B = scales::rescale(crit_1_1_B_raw, to = c(0, 1))) %>%
  # Combined LSF nearshore exposure indicator weighted for representation of each LSF type 
  mutate( crit_1_1 = ais_share_global * crit_1_1_A + nonbroadcasting_share_global * crit_1_1_B )

crit_1_1_data %>%
  ggplot()+
  geom_histogram(aes(x=crit_1_1))

ggplot(crit_1_1_data, aes(x =crit_1_1_A , y =   crit_1_1_B, color = crit_1_1)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method= "lm")+
  scale_color_viridis_c(option = "magma", direction = -1) +
  geom_text_repel(
    aes(label = country_name),
    size = 3,
    max.overlaps = Inf,
    box.padding  = 0.25,
    point.padding = 0.2
  ) +
  labs(
    x = "AIS broadcasting component (crit_1_1_A)",
    y = "Non-broadcasting component (crit_1_1_B)",
    color = "crit_1_1 score",
    title = "Exploring Exposure Components: AIS vs Non-broadcasting Density"
  ) +
  theme_classic() +
  theme(text = element_text(size = 14))


shapiro.test(crit_1_1_data$crit_1_1)

### 1_2_A Contribution to global marine SSF production (scaled to nearshore area , IHH) #####
#Description: Percent contribution of countries’ small-scale fisheries production to global small-scale fisheries production


#####Calculate SSF catch scaled relative to nearshore area for each country ###########
#THIS DATA IS CONFIDENTIAL AND CANNOT BE SHARED. Calculation details remain here to clarify how the data was used in the analysis.  

ssf_catch<- read.csv( here("data", "raw", "ssf_catch.csv"), sep=",", header=TRUE) %>% 
  filter(Marine_Inland_char=="Marine") %>% 
  rename( Alpha.3.code=country_ISO_alpha3) %>% 
  filter(Alpha.3.code %in% country_list) %>% 
  group_by(country, Alpha.3.code) %>% 
  slice_head(n=1) %>%
  dplyr::select(Alpha.3.code,country,national_catch_final ) %>% 
  ungroup() %>% 
  drop_na()

ssf_catch$national_catch_final<-round(ssf_catch$national_catch_final)

# gfw_data already contains the correct area_km2 from the nearshore fishing calc
gfw_area <- gfw_data %>%  
  dplyr::select(Alpha.3.code, nearshore_area)  # pull only the area field

# Join SSF catch to GFW area
ssf_catch_area <- ssf_catch %>% 
  left_join(gfw_area, by = "Alpha.3.code" )%>%  drop_na()

crit_1_2_data<- ssf_catch_area %>% 
  mutate(catch_per_area= log(national_catch_final)/log(nearshore_area)) %>% 
  rename(crit_1_2_raw= catch_per_area) %>% 
  mutate( crit_1_2 = scales::rescale(crit_1_2_raw, to = c(0, 1)) )%>% 
  filter(crit_1_2!="NA")

# Plot a histogram of normalized_crit_2_1_A
crit_1_2_histogram<-ggplot(crit_1_2_data) +
  geom_histogram(aes(x = crit_1_2)) 

shapiro.test(crit_1_2_data$crit_1_2)

### Aggregate exposure variables #####

### CALCULATE EXPOSURE ######

exposure_data<- crit_1_1_data %>% 
  left_join(crit_1_2_data, by =c( "Alpha.3.code")) %>% 
  drop_na() %>% 
  mutate(exposure_raw = rowMeans(dplyr::select(., crit_1_1, crit_1_2 ), na.rm = TRUE)) %>% 
  mutate(exposure_scaled=scales::rescale(exposure_raw)) %>% 
  dplyr::select(Alpha.3.code,
                fishing_ratio,
            crit_1_1, crit_1_1_A, crit_1_1_B, crit_1_2 ,exposure_scaled) 

shapiro.test(exposure_data$exposure_scaled)

ggplot(exposure_data) +
  geom_histogram(aes(x = exposure_scaled)) 

#check correlations
cor(exposure_data$crit_1_1,exposure_data$exposure_scaled)

ggplot(exposure_data) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = -crit_1_2), fill = "blue", position = "dodge") +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = crit_1_1), fill = "pink", position = "dodge") +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), y = exposure_scaled) ,fill = "green", position = "dodge") +
  coord_flip()

ggplot(exposure_data) +
  geom_point(aes(x =  crit_1_1, y = crit_1_2, color = exposure_scaled ))
  


# join exposure data onto world geometry
world_exposure <- world_shp_df %>%
  left_join(exposure_data, by = "Alpha.3.code")

#ais broadcasting map
world_exposure %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf( aes( fill= crit_1_1_A), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,  limits=c(0,1), breaks=c(0,0.5,1))+
  labs( fill="Nearshore density of AIS-broadcasting vessels") +
  theme_classic()+
  theme(legend.position="top",
        legend.key.size = unit(0.5, "cm"))

#non broadcasting map
world_exposure %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf( aes( fill= crit_1_1_B), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,  limits=c(0,1), breaks=c(0,0.5,1))+
  labs( fill="Nearshore density of non-broadcasting vessels") +
  theme_classic()+
  theme(legend.position="top",
        legend.key.size = unit(0.5, "cm"))



A<-  world_exposure %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf( aes( fill= crit_1_1), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,  limits=c(0,1), breaks=c(0,0.5,1))+
  labs( fill="Nearshore density of LSF") +
  theme_classic()+
  theme(legend.position="top",
        legend.key.size = unit(0.5, "cm"))
A
B<-  world_exposure %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf(aes( fill= crit_1_2), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,limits=c(0,1), breaks=c(0,0.5,1))+
  labs(fill="SSF catch (kg) / nearshore area km^2") +
  theme_classic()+  
  theme(legend.position="top")
B
C<-  world_exposure %>% 
  ggplot() +
  geom_sf(data=world_shp, fill="lightgrey", color="white")+
  geom_sf( aes( fill= exposure_scaled), color="lightgrey") +
  scale_fill_viridis_c(option="magma", direction=-1,limits=c(0,1), breaks=c(0,0.5,1))+
  labs(fill="Exposure") +
  theme_classic()+  
  theme(legend.position="top")
A+B+C

#ggsave("Fig_2_exposure.tiff", dpi=300, height=4, width=18)

#check variable influence

reshaped_exposure_data <- exposure_data %>%
  st_drop_geometry() %>% 
  dplyr::select(crit_1_1, crit_1_2 ,exposure_scaled ) %>% 
  gather(key = "variable", value = "value") %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))


exposure_means<-ggplot(reshaped_exposure_data, aes(x = variable, y = mean)) +
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
  stringsAsFactors = FALSE) %>% 
  rename(Alpha.3.code=country_ISO_alpha3 ) %>% 
  filter(Alpha.3.code %in% country_list) 

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
  stringsAsFactors = FALSE) %>%  
  rename(Alpha.3.code=country_ISO_alpha3 ) %>% 
  filter(Alpha.3.code %in% country_list) 


crit_2_1_B_data<-employment_data %>% 
  dplyr::select(country,Alpha.3.code ,   harvest_marine_SSF, harvest_marine_LSF) %>% 
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
  dplyr::select(country,Alpha.3.code ,   harvest_marine_SSF)

ssf_employment$harvest_marine_SSF<-as.numeric(ssf_employment$harvest_marine_SSF)

labor_data<- read.csv( 
  here("data", "world_bank_employment_data.csv"),
  stringsAsFactors = FALSE) %>% 
  rename(Alpha.3.code=Country.Code ) %>% 
  filter(Alpha.3.code %in% country_list) 

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
  dplyr::select(country,Alpha.3.code ,   Marine_harvest_WOC_SSF ) %>% 
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
  left_join(crit_2_1_B_data, by =c( "Alpha.3.code")) %>% 
  left_join(crit_2_1_C_data, by =c( "Alpha.3.code")) %>% 
  left_join(crit_2_1_D_data, by =c( "Alpha.3.code")) %>% 
  mutate(Subregion = countrycode(
    Alpha.3.code, 
    origin = "iso3c", 
    destination = "un.regionsub.name"), #assign region for imputing later
    Subregion = ifelse(Alpha.3.code == "TWN", "Eastern Asia", Subregion)
  ) %>%
  mutate(Continent = countrycode(
    Alpha.3.code, 
    origin = "iso3c", 
    destination = "continent") #assign region for imputing later
  ) %>%
  dplyr::select(Alpha.3.code, Subregion,Continent,
                crit_2_1_A,
                crit_2_1_B,
                crit_2_1_C,
                crit_2_1_D ) %>%
  filter(rowSums(across(c(crit_2_1_A, crit_2_1_B, crit_2_1_C, crit_2_1_D), ~ is.na(.))) <= 2) #remove countries missing more than 2 data points



### 2_2 Composite index of coastal nutritional dependence on SSFs ######
### 2_2_A Contribution to marine SSF production within country (kg/  coastal cap /yr, IHH) [for coastal population within 100 km]####

coastal_population<-read.csv( 
  here("data", "coastal_population.csv"),
  stringsAsFactors = FALSE) %>% 
  rename(Alpha.3.code =country_ISO_alpha3) %>% 
  filter(Alpha.3.code %in% country_list)

crit_2_2_A_data<- ssf_catch %>% 
  dplyr::left_join(coastal_population, by = c("Alpha.3.code") ) %>% 
  filter(coastal_population!=0) %>% 
  mutate(ssf_catch_kg = national_catch_final * 1000) %>% 
  mutate(crit_2_2_A_raw= ssf_catch_kg/coastal_population ) %>% 
  mutate(crit_2_2_A_log= log(ssf_catch_kg)/log(coastal_population )) %>% 
  mutate(crit_2_2_A = scales::rescale(crit_2_2_A_log) ) 

# plot
crit_2_2_A_histogram<-ggplot(crit_2_2_A_data) +
  geom_histogram( aes(x =crit_2_2_A))

#2.2_B Quality: Nutrient supply of catch (iron, zinc, calcium, vitamin A) for coastal residents [compiled domestic proportions that would meet 25% RDI for coastal population]

nutrition_data<-  read.csv( 
    here("data", "nutrition_data.csv"),
    stringsAsFactors = FALSE) %>% 
  filter(marine_inland_char=="marine") %>% 
  mutate(Continent = countrycode(
    sourcevar = country,
    origin = "iso3c",
    destination = "continent"
  )) %>% 
  rename(Alpha.3.code =country) %>% 
  filter(Alpha.3.code %in% country_list) 

crit_2_2_B_data<- nutrition_data %>% 
  dplyr::left_join(coastal_population, by = c("Alpha.3.code") ) %>% 
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
                           rename(Alpha.3.code=ISO3) %>% 
  filter(Alpha.3.code %in% country_list) %>% 
  dplyr::select(Alpha.3.code,  PIMII.without.Fortification) %>% 
  rename(crit_2_2_C_raw =PIMII.without.Fortification) %>% 
  mutate(crit_2_2_C_log=log(crit_2_2_C_raw+1)) %>% 
  mutate(crit_2_2_C = scales::rescale(crit_2_2_C_log) ) 

# plot
crit_2_2_C_historgram <-ggplot(crit_2_2_C_data) +
  geom_histogram( aes(x = crit_2_2_C))

shapiro.test(crit_2_2_C_data$crit_2_2_C)


### Aggregate 2_2 variables ######
crit_2_2_data <-crit_2_2_A_data %>%
  left_join(crit_2_2_B_data, by =c( "Alpha.3.code")) %>%
  left_join(crit_2_2_C_data, by =c( "Alpha.3.code")) %>%
  mutate(Subregion = countrycode(
    Alpha.3.code, 
    origin = "iso3c", 
    destination = "un.regionsub.name"), #assign region for imputing later
    Subregion = ifelse(Alpha.3.code== "TWN", "Eastern Asia", Subregion)
  ) %>%
  mutate(Continent = countrycode(
    Alpha.3.code, 
    origin = "iso3c", 
    destination = "continent") #assign region for imputing later
  ) %>%
  dplyr::select(Alpha.3.code, Subregion,Continent,
                crit_2_2_A_raw,
                crit_2_2_B_raw,
                crit_2_2_C_raw,

                crit_2_2_A,
                crit_2_2_B ,
                crit_2_2_C) %>%
  filter(
    !rowSums(is.na(.)) > 2)  # Keep countries with 2 or fewer missing values


### CALCULATE SENSITIVYTY INDEX (before imputation, see below). We don't impute here because we want to have all the other data for possible countries based on data availability for exposure and adaptive capacity #####
sensitivity_data_original<- crit_2_1_data %>% 
  left_join(crit_2_2_data, by =c( "Alpha.3.code", "Continent", "Subregion"))  %>% 
  dplyr::select(Alpha.3.code, Continent, Subregion,
         
                crit_2_1_A,
                crit_2_1_B,
                crit_2_1_C,
                crit_2_1_D,
                
                crit_2_2_A,
                crit_2_2_B,
                crit_2_2_C
                )


###############################################################################

#################################################################################
# COMPONENT 3. ADAPTIVE CAPACITY 
#################################################################################

### 3_1 HDI ####

hdi<-read.csv( 
  here("data", "hdi_time_series.csv"),
  stringsAsFactors = FALSE) %>% 
  rename(Alpha.3.code=country_ISO_alpha3) %>% 
  dplyr::select(Alpha.3.code, country,
                hdi_2013, hdi_2014, hdi_2015, hdi_2017, hdi_2017) %>% 
  filter(Alpha.3.code %in% country_list) 

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
  rename(Alpha.3.code=country_ISO_alpha3) %>% 
  filter(Alpha.3.code!="") %>% 
  filter(Alpha.3.code %in% country_list) %>% 
  dplyr::select(Alpha.3.code, corruption, gov_effectiveness, political_stability, regulatory_quality, rule_of_law, voice_accountability) %>% 
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
  left_join(crit_3_2_data, by =c( "Alpha.3.code"))  %>% 
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
  dplyr::select(Alpha.3.code,adaptive_capacity,adaptive_capacity_raw, adaptive_capacity_log, 
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
  gather(key = "variable", value = "value", -Alpha.3.code) %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))


adaptive_capacity_means<- ggplot(reshaped_adaptive_capacity_data, aes(x = variable, y = mean)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.8)) +
  theme_classic() +
  labs(title="Adaptive Capacity")+
  scale_y_continuous(limits=c(-0.2, 1))+
 theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
  inner_join(crit_2_2_data,by = c("Alpha.3.code", "Continent", "Subregion")) %>%
  mutate(
    sensitivity = rowMeans(dplyr::select(., crit_2_1, crit_2_2), na.rm = TRUE)
  )

#create a table to show for which countreis data were imputed 


# Columns to check for imputation (exclude '_raw')
crit_cols <- c("crit_2_1_A","crit_2_1_B","crit_2_1_C","crit_2_1_D",
               "crit_2_2_A","crit_2_2_B","crit_2_2_C")

sensitivity_combined <- sensitivity_data %>%
  inner_join(
    sensitivity_data_original %>% dplyr::select(Alpha.3.code, Continent, all_of(crit_cols)),
    by = c("Alpha.3.code","Continent"),
    suffix = c("_final","_orig")
  )

imputed_countries <- sensitivity_combined %>%
  dplyr::select(Alpha.3.code, Continent,
         crit_2_1_A_orig, crit_2_1_B_orig, crit_2_1_C_orig, crit_2_1_D_orig,
         crit_2_2_A_orig, crit_2_2_B_orig, crit_2_2_C_orig) %>%
  pivot_longer(
    cols = ends_with("_orig"),
    names_to = "criterion",
    values_to = "value"
  ) %>%
  filter(is.na(value)) %>%  # keep only originally missing
  mutate(criterion = gsub("_orig", "", criterion)) %>%
  group_by(Continent, criterion) %>%
  summarize(countries = paste(Alpha.3.code, collapse = ", "), .groups = "drop")


#imputation was only done for 2_2_C; all other countries were removed for too much missing data. 
#10 countries received imputed 2_2_C data: 
#Africa: COD, COM, ERI, GNQ, LBY
#Asia: BHR, QAT, SYR
#Oceania: PNG, TON

#taiwan was imputed but it is removed in the next step bc it does not have exposure data. 


######## MERGE 3 INDICES  ###########################
#put all criteria together in one df

all_components <- exposure_data %>% 
  left_join(sensitivity_data, by =c( "Alpha.3.code")) %>% 
  left_join(adaptive_capacity_data, by =c( "Alpha.3.code")) %>% 
  mutate(Continent = countrycode(
    Alpha.3.code, 
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

# Specify all the variables (main components + sub-criteria) for a summary table
all_cols <- c(
  # main indices
  "exposure_scaled", "sensitivity_scaled", "adaptive_capacity_inverse_scaled",
  # sub-criteria
  "crit_1_1", "crit_1_1_A", "crit_1_1_B", "crit_1_2",
  "crit_2_1_A", "crit_2_1_B", "crit_2_1_C", "crit_2_1_D",
  "crit_2_2_A", "crit_2_2_B", "crit_2_2_C",
  "crit_3_1", 
  "crit_3_2_A", "crit_3_2_B", "crit_3_2_C", "crit_3_2_D", "crit_3_2_E", "crit_3_2_F"
)

# Create summary table
summary_table <- all_components %>%
  dplyr::select(all_of(all_cols)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%   mutate(across(where(is.numeric), ~round(., 3)))

summary_table
write.csv(summary_table, "variable_summaries.csv")

#check simple methods of producing the SIFI Index
df <- all_components %>%
  group_by(Alpha.3.code) %>%
  mutate(   overall_index_sum =( exposure_scaled + sensitivity_scaled + adaptive_capacity_inverse_scaled), 
            
            overall_index_prod = (prod(c(exposure_scaled , sensitivity_scaled + adaptive_capacity_inverse_scaled))),
                                        
            overall_index_mean = (mean(c(exposure_scaled , sensitivity_scaled + adaptive_capacity_inverse_scaled)))) %>%

  ungroup() %>%  # Ungroup the data
  mutate(
    index_sum_scaled = scales::rescale(log(overall_index_sum+1), to = c(0, 1)),
    
    index_prod_scaled = scales::rescale(log(overall_index_prod+1), to = c(0, 1)),
   
    index_mean_scaled = scales::rescale(log(overall_index_mean+1), to = c(0, 1))
  ) 


write.csv(df, "data/SIFI_Index_data.csv")

#these are countries for which we have ALL data.above, data are filtered already for these countries only so that 
#they can be correctly scaled. 
# country_list <- df %>%
#   pull(Alpha.3.code) %>%
#   unique()
# # Convert to a dataframe for clean saving
# country_df <- data.frame(Alpha.3.code = country_list)
# 
# # Write to CSV
# write.csv(country_df,
#           file = "country_list_from_all_components.csv",
#           row.names = FALSE)

#PLOT DISTRIBUTIONS ######

# ===== HISTOGRAMS OF CRITERIA (crit_2_1, crit_2_2, etc.) =====
crit_long <- df %>%
  dplyr::select(Alpha.3.code, starts_with("crit_")) %>%
  dplyr::select(-ends_with("_raw")) %>%   # exclude raw columns
  pivot_longer(cols = starts_with("crit_"),
               names_to = "criterion",
               values_to = "value")

crit_plot <- ggplot(crit_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~criterion, scales = "free", ncol = 3) +
  theme_classic(base_size = 16) +
  labs(x = NULL, y = "Count") +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    strip.text = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
crit_plot

#ggsave("crit_distribution.tiff", dpi=300, width=8, height=17)


# Create individual plots for each component
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


plot1 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, adaptive_capacity_scaled), 
               y = adaptive_capacity_scaled, 
               fill = adaptive_capacity_scaled)) +
  color_scale +
  theme_classic() +
  labs(y="Adaptive capacity", x="Country")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot2 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, exposure_scaled), 
               y = exposure_scaled, 
               fill = exposure_scaled)) +
  color_scale +
  coord_flip()+
  labs(y="Exposure", x="Country")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot3 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, sensitivity_scaled), 
               y = sensitivity_scaled, 
               fill = sensitivity_scaled)) +
  color_scale +
  labs(y="Sensitivity", x="Country")+
  theme_classic() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")

plot4 <- ggplot(df) +
  geom_col(aes(x = fct_reorder(Alpha.3.code, -index_prod_scaled), 
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



#ggsave("component_distribution.tiff", dpi=300, width=8, height=17)



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
ggplot(df, aes(x = fct_reorder(Alpha.3.code,index_prod_scaled),y = index_prod_scaled )) +
  geom_rect(
    xmin = -Inf, xmax = Inf, ymin = threshold, ymax = 1,
    fill = "lightgray", aes(alpha = 0.9 ) # Shaded rectangle for top quartile
  ) + 
  geom_point(aes(color = Alpha.3.code %in% highlight_countries)) +  # Highlight countries by color
  geom_hline(yintercept = quartiles, color = "blue", linetype = "dashed") +  # Add horizontal lines for quartiles
  geom_text(   aes(label = ifelse(Alpha.3.code %in% highlight_countries, Alpha.3.code, "")),
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

