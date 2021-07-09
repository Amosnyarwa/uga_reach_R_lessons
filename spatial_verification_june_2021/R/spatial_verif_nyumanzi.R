# Load libraries

library(sf)
library(tidyverse)
library(butteR)

# Load data

df_collect_data <- read_csv(file = "inputs/REACH_UGA_HLP_raw_dataset.csv") %>% 
  filter(
    refugee_settlement == "nyumanzi"
  )

df_collect_data_pts <- st_as_sf(df_collect_data, coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)

# st_transform(crs = 32636)

settlement_limits <- st_read("inputs", "Nyumanzi_settlement", quiet = TRUE) %>% st_transform(crs = 4326)
refugee_sample_pts <- st_read("inputs", "Nyumanzi_HLP_pts", quiet = TRUE) %>% st_transform(crs = 4326)

# map of interviews, sample pts and limits

ggplot()+
  geom_sf(data = settlement_limits)+
  geom_sf(data = refugee_sample_pts, color = "red")+
  geom_sf(data = df_collect_data_pts, size = 1.2, color = "darkgreen")+
  theme_bw()+
  # geom_smooth()+
  labs(
    title = "HLP Complete Spatial Verification - Nyumanzi"
  )

# check_point_distance_by_id

find_some_dist <- check_point_distance_by_id(sf1=df_collect_data_pts, sf2=refugee_sample_pts, sf1_id = "point_number", sf2_id = "OBJECTID",dist_threshold = 150 )
my_data <- find_some_dist$dataset

 # filter points with distance greater than 150 meters

df_points_above_threshold <- my_data %>% 
  filter(
    dist_m > 150) %>% 
  select(
    point_number)

write_csv(x = df_points_above_threshold, file = "outputs/point_dist_above_threshold.csv")








