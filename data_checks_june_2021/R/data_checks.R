# getting started with data checks

library(tidyverse)
library(lubridate)

# load dataset

df_survey_data <- read_csv("Inputs/UGA2101a_Rapid_Assessment_on_Livelihood_and_Market_Strengthening_Opportunities_Feb2021.csv")

# some data investigation

dim(df_survey_data)
nrow(df_survey_data)
ncol(df_survey_data)
colnames(df_survey_data)

#check available dates

df_available_date <- df_survey_data %>%
  select(today) %>% 
  arrange(as_date(today)) %>% 
  unique()


# filter data for a particular date
df_data_for_a_date <- df_survey_data %>% 
  filter(today=="27-02-21")

# calculate time for the survey

df_survey_time <- df_data_for_a_date %>% 
  mutate(
    int.survey_time = difftime(end, start, units = "mins"),
    int.survey_time = round(int.survey_time, 2)
  )

# calculate time diff btn surveys

diff_time_btn_surveys <- df_data_for_a_date %>% 
  mutate(enumerator_id = ifelse(!is.na(enum_id_refugee), enum_id_refugee, enum_id_host)) %>% 
  group_by(
   today, enumerator_id 
  ) %>% 
  arrange(start, .by_group = TRUE) %>% 
  mutate(
    t_between_survey = 
  )

