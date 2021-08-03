# Load libraries

library(sf)
library(tidyverse)


# Load data

df_collect_data <- read_csv(file = "inputs/REACH_UGA_HLP_raw_dataset.csv") %>% 
  filter(
    refugee_settlement == "nyumanzi"
  )

df_collect_data_pts <- st_as_sf(df_collect_data, coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)

# st_transform(crs = 32636)

settlement_limits <- st_read("inputs", "Nyumanzi_settlement", quiet = TRUE) %>% st_transform(crs = 4326)
refugee_sample_pts <- st_read("inputs", "Nyumanzi_HLP_pts", quiet = TRUE) %>% st_transform(crs = 4326)

# Find distances
calc_distances <- data.frame()

pt_nos_sample <- refugee_sample_pts %>% 
  pull(
    OBJECTID 
  ) %>% 
  unique()

for (pt_no in pt_nos_sample) {
  df_current_sample <- refugee_sample_pts %>% 
    filter(
      OBJECTID == pt_no
    )
  df_current_field_data <- df_collect_data_pts %>% 
    filter(
      point_number == pt_no
    ) %>% 
    mutate(uuid =`_uuid`, latitude = sf::st_coordinates(.)[,1],  longitude = sf::st_coordinates(.)[,2])
  if (nrow(df_current_field_data)>=1){
    current_distance <- st_distance(df_current_sample, df_current_field_data, by_element = TRUE)
    # print(current_distance)
    print(nrow(df_current_field_data))
    print(typeof(current_distance))
    df_current_data_with_distance <- as_tibble (df_current_field_data) %>% 
      dplyr::select(-geometry) %>% 
      mutate(
        distance = current_distance
      ) %>% 
      dplyr::select(uuid, today, enumerator, point_number, refugee_settlement,longitude,latitude, distance)
    calc_distances <- rbind(calc_distances, df_current_data_with_distance)}
  
}



# data frame for spatial verification
# check_spatial_verification <- data.frame()
# df_spatial_verification <- df_collect_data %>% 
#   select("_uuid", "today", "enumerator", "point_number", "refugee_settlement", "_geopoint_longitude","_geopoint_latitude") %>% 
#   mutate(calc_distances)

# write_csv(x = df_spatial_verification, file = "outputs/spatial_verification.csv")

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

