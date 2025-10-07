### global map for nearshore fishing eff

rm(list=ls())
library(scales)
library(dplyr)
library(likert)
library(maps)
library(cartogram)
library(scales)
library(ggplot2)
library(tidyverse)
#library(tidyr)
library(viridis)
library(ggrepel)
library(table1) 
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




# # World polygons from the maps package
world_shp <- sf::st_as_sf(maps::map("world", plot = T, fill = TRUE))
# 
# load fishing data, which is constrained to country's square km within 25km

setwd("/Users/melissacronin/Desktop/global_GFW_data/2019_25KMBUFF_Effort")
shapefile_global <- st_read("2019_25KMBUFF_Effort.shp")
#
# # Extract latitude and longitude from the geometry
#shapefile_global <- st_transform(effort, crs = 4326)  # Ensure data is in WGS 84
#shapefile_global$latitude <- st_coordinates(shapefile_global)[, "Y"]
# shapefile_global$longitude <- st_coordinates(shapefile_global)[, "X"]
# #
# # #### collect fishing data in cells to a sum ####
# fishing_aggregated_data <- shapefile_global %>%
#   group_by(latitude, longitude) %>%
#   summarize(fishing_hours = sum(fishing_ho, na.rm = T))
# 
# fishing_aggregated_df<- fishing_aggregated_data %>% 
#   st_drop_geometry()

setwd("/Users/melissacronin/Desktop/global_GFW_data/2019_25KMBUFF_Effort")
#write.csv(fishing_aggregated_df, "aggregated_nearshore_fishing_Dec4.csv")

# # Preprocess the fishing_aggregated data to create a new variable with cut categories
fishing_aggregated_data<- read.csv("aggregated_nearshore_fishing_Dec4.csv", sep=",", header=T)


max(fishing_aggregated_data$fishing_hours)

fishing_aggregated_for_plot <- fishing_aggregated_data %>%
  mutate(fishing_hours_category = cut(fishing_hours,
                                      breaks = c(-Inf, 100, 1000, 10000,  100000, 500000, Inf),
                                      labels = c("<100", "<1,000", "<10,000", "<100,000", "<500,000", "500,000+")))



# Sort the fishing_aggregated data by fishing_hours in descending order
fishing_aggregated_for_plot <- fishing_aggregated_for_plot[order(fishing_aggregated_for_plot$fishing_hours_category), ]


viridis_palette <- viridisLite::viridis(4, begin = 0.8, end = 0)
viridis_palette <- viridis_palette[-4]  # Exclude yellow color

fishing_plot<- 
  ggplot() +
  geom_sf(data = world_shp , fill = "white", color = "grey") +
  geom_tile(data = fishing_aggregated_for_plot, aes(x = longitude, y = latitude, fill = fishing_hours_category), width = 0.5, height = 0.5) +
  theme_minimal() +  #
  labs(title="A", fill = "Fishing Hours", x="", y ="")+
  theme(text=element_text(size=18))+
  theme(legend.position = "none", text = element_text(size = 16)) +
  # theme(legend.key.size = unit(1, "lines"), legend.title.align = 0.5,
  #       legend.text = element_text(size = 15),
  #       legend.title = element_text(size = 16, face = "bold"),
  #       legend.spacing = unit(0.2, "lines"))+
  theme(legend.position="top")#+

#  annotation_scale(location = "br", width_hint = 0.15) +  # Add scale bar
 # annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering())  # Add compass rose 

fishing_plot
library(rnaturalearth)
world <- ne_countries(returnclass = "sf")



##ADD SSF CATCH HERE !!
setwd("/Users/melissacronin/Desktop/global_GFW_data/Fishing25km")
crit_1_1_data<-read.csv( "crit_1_1_data.csv", sep=",", header=T)
merged_data <- merge(world, crit_1_1_data, by.x = "iso_a3", by.y = "Alpha.3.code", all.x = TRUE) %>% 
  filter(adm0_a3!="CHN")
head(merged_data)
setwd("/Users/melissacronin/Desktop/IHH/IHH_country_criteria_prioritization")
crit_1_2_df<- read.csv("crit_1_2_data.csv", header=T, sep=",")
merged_crit_1_2 <- merge(world, crit_1_2_df, by.x = "iso_a3", by.y = "Alpha.3.code", all.x = TRUE) 


sf_palette <- viridis::viridis(7, option = "plasma")
tile_palette <- viridis::viridis(4, option = "inferno")


ssf_catch<-ggplot() +
  geom_sf(data=merged_crit_1_2, aes(fill=crit_1_2), color="lightgrey") +
  scale_fill_gradientn(colors = sf_palette, na.value = "grey",    labels = scales::number_format(scale = 1, accuracy = 0.1)
  ) +
  theme_bw()+
  theme(legend.position="top")+
  labs(title="A",fill="Nearashore SSF catch (kg/km2)")+
  theme(text=element_text(size=14))


LSF_fishing <- ggplot() +
  geom_sf(data=world, fill="grey", color="lightgrey")+
  geom_tile(data = fishing_aggregated_for_plot, aes(x = longitude, y = latitude, fill = fishing_hours_category), width =1, height =1) +
  scale_fill_manual(values = sf_palette) +
  theme_bw()+
  theme(legend.position="top")+
  labs(title="B", fill="Nearshore LSF effort (hours)")+
  theme(text=element_text(size=14))


ssf_catch / LSF_fishing

ggsave("Figure_1_AB_Dec5.tiff", dpi=300, height=10, width=8)

#global characteristsics of data
#### collect fishing data in cells to a sum ####
fishing_aggregated_data_flag <- shapefile_global %>%   
  group_by( ISO_TER1) %>% 
  summarize(fishing_hours = sum(fishing_ho, na.rm = T)) 

setwd("/Users/melissacronin/Desktop/clean_ihh_project_data/buffer_25km_GIS")
country_nearshore_area<- read.csv("coastal_buffer_iso.csv", header=T, sep=",") %>% 
  dplyr::select(-X) %>%  rename(ISO_TER1=iso_code)


country_fishing_scaled<- fishing_aggregated_data_flag %>% 
  left_join(country_nearshore_area, by = "ISO_TER1") %>% 
  mutate(fishing_scaled= fishing_hours/sum_area )%>% 
  drop_na() %>% 
  st_drop_geometry()


world_shp$ISO_TER1 <- countrycode(sourcevar = world_shp$ID, origin = "country.name", destination = "iso3c") 

joined_scaled_fishing <- merge(world_shp, country_fishing_scaled, by= "ISO_TER1", all.x = TRUE) %>% 
  drop_na() 

scaled_fishing_aggregated <- joined_scaled_fishing %>%
  mutate(scaled_fishing_category = cut(fishing_scaled,
                                      breaks = c(-Inf, 5, 10,25, 50,   Inf),
                                      labels = c("<5","<10", "<25", "<50", "50+" )))



# Sort the fishing_aggregated data by fishing_hours in descending order
scaled_fishing_aggregated_for_plot <- scaled_fishing_aggregated[order(scaled_fishing_aggregated$scaled_fishing_category), ]


ggplot() +
  geom_sf(data = world_shp , fill = "white", color = "grey") +
  geom_sf(data = joined_scaled_fishing , aes(fill=fishing_scaled))+
  #geom_sf(data = scaled_fishing_aggregated_for_plot , aes(fill=scaled_fishing_category))+
  theme_classic() +  #
  scale_fill_gradient(low = "lightgrey", high = "black") +
  geom_tile(data = fishing_aggregated_for_plot, aes(x = longitude, y = latitude,color = fishing_hours_category), width = 0.5, height = 0.5) +
  labs(title="A", fill = "Fishing Hours", x="", y ="")+
  scale_color_viridis_d()+
  theme(text=element_text(size=18))+
  theme(legend.position = "none", text = element_text(size = 16)) +
  theme(legend.position="top")


ggsave("double_map.tiff", dpi=300)

setwd("/Users/melissacronin/Desktop/clean_ihh_project_data")
write.csv(country_fishing_scaled)

top_20 <- country_fishing_scaled %>%
  top_n(20, fishing_scaled) %>%   # Select the top 50 ISO_TER1 by fishing hours
  mutate(ISO_TER1 = case_when(
    ISO_TER1 == "ASM" ~ "American Samoa",
    ISO_TER1  == "BHR" ~ "Bahrain",
    ISO_TER1 == "FRO" ~ "Faroe Islands",
    ISO_TER1 == "FSM" ~ "Micronesia",
    ISO_TER1 == "GGY" ~ "Guernsey",
    ISO_TER1 == "GRD" ~ "Grenada",
    ISO_TER1 == "JEY" ~ "Jersey",
    ISO_TER1 == "KIR" ~ "Kiribati",
    ISO_TER1 == "KOR" ~ "South Korea",
    ISO_TER1 == "MCO" ~ "Monaco",
    ISO_TER1 == "MHL" ~ "Marshall Islands",
    ISO_TER1 == "MLT" ~ "Malta",
    ISO_TER1 == "MUS" ~ "Mauritius",
    ISO_TER1 == "NLD" ~ "Netherlands",
    ISO_TER1 == "PLW" ~ "Palau",
    ISO_TER1 == "REU" ~ "Réunion",
    ISO_TER1 == "SGP" ~ "Singapore",
    ISO_TER1 == "SHN" ~ "St Helena, Ascension and Tristan da Cunha",
    ISO_TER1 == "SPM" ~ "Saint Pierre and Miquelon",
    ISO_TER1 == "SYC" ~ "Seychelles",
    ISO_TER1 == "GUM" ~ "Guam",
    ISO_TER1 == "TWN" ~ "Taiwan",
    ISO_TER1 == "KNA" ~ "St. Kitts and Nevis",
    ISO_TER1 == "DNK" ~ "Denmark",
    TRUE ~ as.character(ISO_TER1)  # Keep other values unchanged
  )) 

top_20$ISO_TER1


# nearshore fishing density 
C<-ggplot(top_20 , aes(x = fct_reorder(ISO_TER1,-fishing_scaled), y= fishing_scaled)) +
  geom_col()+
  theme_classic() +
  scale_y_continuous(limits=c(0,500), expand = c(0, 0))+
  labs(title = "C",  x = "Country", y ="LSF fishing hours (2019) / nearshore area (km2)") +
  theme(text = element_text(size = 18)) +
  theme(legend.position = "top") +
  coord_flip()+
  theme(legend.text = element_text(size = 15))  # You can modify additional theme settings if needed



ggsave("fishing_intensity.tiff", dpi=300, height=8, width=5)
#GEAR
#### collect fishing data in cells to a sum ####
fishing_aggregated_data_gear <- shapefile_global %>%   
  group_by(vessel_cla) %>% 
  summarize(fishing_hours = sum(fishing_ho, na.rm = T)) 

gear_data_clean<- fishing_aggregated_data_gear %>% 
  mutate(vessel_cla = case_when(
    vessel_cla == "purse_seines" ~ "purse seine",
    vessel_cla == "tuna_purse_seines" ~ "purse seine",
    vessel_cla == "other_purse_seines" ~ "purse seine",
    vessel_cla == "other_seines" ~ "seiners",
    vessel_cla == "set_longlines" ~"longline",
    vessel_cla == "drifting_longlines" ~"longline",
    TRUE ~ vessel_cla  # Keep other values unchanged
  )) %>% 
  mutate(vessel_cla = gsub("_", " ", vessel_cla)) %>% 
  group_by(vessel_cla) %>% 
  summarise(total_hours=sum(fishing_hours))

library(scales)

B<-ggplot(gear_data_clean, aes(x = fct_reorder(vessel_cla,total_hours), y= total_hours)) +
  geom_col()+
  scale_y_continuous(labels = label_number_si(),expand = c(0, 0))+
  coord_flip()+
  theme_classic() +
  labs(title="B", x = "Gear", y = "Nearshore fishing hours (2019)") +
  theme(text = element_text(size = 18)) +
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = 15))  # You can modify additional theme settings if needed

bars<-B+C


plot_grid(
  fishing_plot, 
  bars, 
  ncol = 1)

plot_grid(fishing_plot, bars, align = "v", ncol=1)


ggsave("Fig_1_LSF_activity_map_combined.tiff", dpi=300, width=13, height=10)
