
library(covid19mobility) # get mobility data from Apple and Google
library(dplyr) # for joining, piping data frames
library(magrittr)
library(lubridate) # for dealing with our date data
library(sf) # shapefile manipulation
library(urbnmapr) #to get county and state data in a shapefile
library(gganimate) # to animate plots
library(ggplot2)
library(viridis)

mexico_city_mobility <- refresh_covid19mobility_google_subregions() %>%
  filter(location == "Mexico City") %>%
  mutate(score = ifelse(value > 50,
                        2, 0)) %>%
  tail(6)

saveRDS(mexico_city_mobility, "data/mexico_city_mobility.rds")

x <- refresh_covid19mobility_google_subregions()

y <- read.csv("/Users/carinapeng/Downloads/Global_Mobility_Report (1).csv")

haiti_october <- y %>%
  filter(country_region == "Haiti") %>%
  filter(grepl("2020-10",date))
  

haiti_october$sub_region <- paste(haiti_october$sub_region_1, 
                                  haiti_october$sub_region_2, 
                                  haiti_october$metro_area)
  

haiti_october %>%
  ggplot(aes(x = date, y = workplaces_percent_change_from_baseline, group = sub_region, color = sub_region)) +
  geom_line(size = 1, alpha = 1) +
  facet_wrap(~sub_region, scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  scale_color_viridis(discrete=TRUE, option = "cividis") +
  labs(color = "Département",
       title = "Changement en pourcentage de la mobilité en octobre, par département",
       subtitle = "pour raisons de travail depuis la semaine épidémiologique #7, comparé aux mouvements enregistrés en janvier 2020") +
  ylab("Changement (%) en raison du travail") +
  xlab("Date") 



