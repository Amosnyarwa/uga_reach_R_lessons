# extracting some information for Dave to check with his values

library(tidyverse)

df_jmmi_ref <- read_csv("inputs/202003_market_monitoring_cleaned.csv")

df_jmmi_ref_subset <- df_jmmi_ref %>% 
  select(settlement, price_r_pads, price_counter_book,
         price_blanket, price_sauce_pan,
         price_nails
         ) %>% 
  group_by(settlement) %>% 
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
  ) %>% 
  mutate(settlement = str_to_lower(settlement))

# location ----------------------------------------------------------------

settlement_data <- read_csv("inputs/settlement_list.csv", na = c(""," ", "NA"))
district_data <- read_csv("inputs/Districts_list.csv", na = c(""," ","NA"))

# settlement data with settlement names in lower case
settlement_data <- settlement_data %>% 
  rename(settlement = NAME0) %>% 
  mutate(settlement = str_to_lower(settlement))

# district data with district names in lower case
district_data <- district_data %>%  mutate(district = str_to_lower(DName2019)) %>% 
  select(F15Regions, district)


df_jmmi_ref_subset_settlement <- df_jmmi_ref_subset %>% 
  left_join(settlement_data, by = "settlement") %>% 
  select(-c(TYPE :   Latitude), -OBJECTID ) %>% 
  rename(country = NAME) %>% 
  mutate(
    country = "uganda",
    DISTRICT = ifelse(settlement == "adjumani", "adjumani", DISTRICT),
    DISTRICT = ifelse(settlement == "rhino", "arua", DISTRICT)
  ) %>% 
  left_join(district_data, by = c("DISTRICT" = "district")) %>% 
  rename(region = F15Regions, district = DISTRICT) %>% 
  select(country, region, district, everything()) %>% 
  mutate(
    region = case_when(
      region %in% c("WEST NILE", "ACHOLI", "BUNYORO") ~ "west nile",
      region %in% c("ANKOLE", "TORO") ~ "south western",
      district == "hoima" ~ "south western",
      TRUE ~ region
    ),
    region = case_when(
      district %in% c("hoima", "masindi") ~ "south western",
      TRUE ~ region
    )
  )  


# write output ------------------------------------------------------------

write_csv(df_jmmi_ref_subset_settlement, "outputs/mean_price_of_some_commodities_by_settlement.csv")
