
library(covid19mobility) # get mobility data from Apple and Google
library(dplyr) # for joining, piping data frames
library(lubridate) # for dealing with our date data
library(sf) # shapefile manipulation
library(urbnmapr) #to get county and state data in a shapefile
library(gganimate) # to animate plots

mexico_city_mobility <- refresh_covid19mobility_google_subregions() %>%
  filter(location == "Mexico City") %>%
  tail(6)

saveRDS(mexico_city_mobility, "data/mexico_city_mobility.rds")
