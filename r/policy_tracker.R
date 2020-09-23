
library(googlesheets4)
library(magrittr)
library(dplyr)

phsm_dribble <- googledrive::drive_get("PHSM_WHO")

data <- phsm_dribble %>%
  read_sheet()

saveRDS(data, "data/who_phsm.rds")



