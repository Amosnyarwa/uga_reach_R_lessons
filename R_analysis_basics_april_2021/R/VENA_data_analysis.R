# load libraries
library(tidyverse)

# load data
df_data <- read_csv(file = "inputs/VENA HH data cleaned_without ID.v9_withoutPSN.csv")

# check data
dim(df_data)
colnames(df_data)
class(df_data)

# filter data
df_from_start_date <- df_data %>% 
  filter(start == "04-Sep-19")

# calculate mean hh size
settlement_mean_hh_size <- df_data %>% 
  group_by(Settlement) %>% 
  summarise(
    mean_hh_size = mean(HH_size_VENA, na.rm=TRUE) %>% 
      round(2)
  )


df_child_labour <- df_data %>% 
  select(contains("child_harsh_PSN"), Settlement)

# summarise
df_ch <- df_child_labour %>% 
  group_by(Settlement) %>%
  mutate(across(where(is.character), as.numeric)) %>% 
  rowwise() %>% 
  mutate(total_psn = sum(child_harsh_PSN_1_labor_heavy_loads: child_harsh_PSN_4_labor_exposure_to_extreme_heat, na.rm = TRUE)) %>% 
  summarise(total_psn_per_settlement = sum(total_psn, na.rm = TRUE))

