library(sf)
library(dplyr)
library(butteR)

# READ IN ALL OF THE DATA (KML, SHAPEFILE, ASSESSMENT DATA) ---------------

# Dont worry all sesnitive information has been removed and the uuids and all data randomized
df<-read.csv("input/HH_HC_MSNA_random_anonymous.csv", stringsAsFactors = FALSE, na.strings = c("", " "))

# look at gis layers to grab the correct layer name
st_layers("input/gis")


#kml files are a little different than shape files, because the data source name (dsn) is actually the kml file and the layer is inside.
#check the layer inside
st_layers("input/gis/HC_MSNA_TargetPoints.kml")

#read in the target points
target_points<-st_read(dsn = "input/gis/HC_MSNA_TargetPoints.kml", layer = "HC_MSNA_TargetPoints" )


Bidibidi_settlement<-st_read(dsn = "input/gis/Bidibidi_settlement.shp", layer = "Bidibidi_settlement" )


plot(Bidibidi_settlement)





# FIND POINTS 25 M AWAY FROM CLOSEST TARGET POINT -------------------------


# now we can check all of the assessment points and see which ones were above 25 m away.

points_more_than_25m_from_target<-butteR::check_distances_from_target(dataset = df,
                                                                      target_points = target_points,
                                                                      dataset_coordinates = c("X_gps_reading_longitude","X_gps_reading_latitude"),
                                                                      cols_to_report = c("X_uuid", "enumerator_id", "union_name"),
                                                                      distance_threshold = 25)

?butteR::check_distances_from_target

# view the results
points_more_than_25m_from_target



# CHECK STRATA OBSERVED AGAINST STRATA RECORDED BY COORDINATE -------------

# load the strata polygon (we can use this to check that they are in the right strata)

strata_polygon<- st_read(dsn = "input/gis",layer = "southern_bgd_adm4_unions", stringsAsFactors=FALSE)

# to check strata you have to make sur that your GIS and assessment data names match
# unfortunately with the tool designed in banglades they do not.... so we will fix that here

#look at names polygon file
strata_polygon$adm4_en
# look at names in dataset
df$union_name

#show names that dont match from data set -- unfortunately none of them do
(df$union_name %>% unique())[df$union_name %>% unique()  %in% strata_polygon$adm4_en==FALSE]



# I would think if i still the labels from the tool rather than the names they might match
# so lets load the tool -- choices, then assessment
choices<-read.csv( "input/tool/HostCommunity_MSNA2019_tool_choices.csv")
survey<-read.csv( "input/tool/HostCommunity_MSNA2019_tool_survey.csv")

#then use koboquest to load_questionnaire
kobo_survey<-koboquest::load_questionnaire(data = df,questions = survey,choices = choices, choices.label.column.to.use ="label..english" )

#now i will question choice names to labels
df_with_choice_labels<-purrr::map2_df(.x= df, .y=colnames(df),.f=kobo_survey$question_get_choice_labels)

#did this solve the problem?
(df_with_choice_labels$union_name %>% unique())[df_with_choice_labels$union_name %>% unique()  %in% strata_polygon$adm4_en==FALSE]

#unfortunatley there are still some that dont match (much less), lets fix this manually here and add the fixed column to the original data
# set as "union_labels"


df$union_labels<-df_with_choice_labels$union_name %>% stringr::str_replace_all( c("Teknaf Pourasabha"="Teknaf Paurashava",
                                                                                  "Ranta Palong" = "Ratna Palong",
                                                                                  "Teknaf Sadar"="Teknaf",
                                                                                  "Baharchara"="Baharchhara"))

#now the shapefile and union labels in the assesssment are harmonized - check
(df$union_labels %>% unique())[df$union_labels %>% unique()  %in% strata_polygon$adm4_en==FALSE]


butteR::check_reported_strata_against_spatial_poly(dataset = df,
                                                   strata_poly = strata_polygon,
                                                   dataset_strata_name = "union_labels",
                                                   poly_strata_name = "adm4_en",
                                                   cols_to_report = "enumerator_id")