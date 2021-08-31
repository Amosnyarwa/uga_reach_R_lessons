# Load libraries

library(sf)
library(tidyverse)
library("ggplot2")

# Load data

df_collect_data <- read_csv(file = "inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.csv") %>% 
  filter(
    settlement_name == "nakivale"
  )


df_collect_data_pts <- st_as_sf(df_collect_data, coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)

# st_transform(crs = 32636)

settlement_limits <- st_read("inputs", "Nakivale_settlement", quiet = TRUE) %>% st_transform(crs = 4326)
refugee_sample_pts <- st_read("inputs", "dfa_Nakivale_Randompts", quiet = TRUE) %>% st_transform(crs = 4326)


# Find distances
calc_distances <- data.frame()

pt_nos_sample <- refugee_sample_pts %>% 
  pull(
    Dscrptn 
  ) %>% 
  unique()

for (pt_no in pt_nos_sample) {
  df_current_sample <- refugee_sample_pts %>% 
    filter(
      Dscrptn == pt_no
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
      dplyr::select(uuid, today, enumerator_id, point_number, settlement_name,longitude,latitude, distance)
    calc_distances <- rbind(calc_distances, df_current_data_with_distance)}
  
}


write_csv(x = calc_distances, file = "outputs/spatial_verification_nakivale_30_08_21.csv")
