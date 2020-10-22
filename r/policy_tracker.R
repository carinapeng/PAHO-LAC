
library(googlesheets4)
library(magrittr)
library(dplyr)

phsm_dribble <- googledrive::drive_get("PHSM_WHO")

data <- phsm_dribble %>%
  read_sheet()

saveRDS(data, "data/who_phsm.rds")

# policy_national <- data %>%
#   filter(country_territory_area == "Chile") %>%
#   filter(admin_level == "national")
# 
# policy_area <- data %>%
#   filter(area_covered == "Santiago")
# 
# policy_join <-
#   rbind(policy_national, policy_area)
# 
# social = ifelse("Social and physical distancing measures" %in% data$who_category,
#                 0,
#                 1)
# isolation = ifelse("Surveillance and response measures" %in% data$who_category,
#                    0,
#                    1)
# support = ifelse("Social and physical distancing measures" %in% data$who_category,
#                  0,
#                  1)
# contact = ifelse("Tracing and quarantining contacts" %in% data$who_category,
#                  0,
#                  1)
# gathering = ifelse("Gatherings, businesses and services" %in% data$who_category,
#                    0,
#                    1)
# disturbance = ifelse("Gatherings, businesses and services" %in% data$who_category,
#                      0,
#                      1)
# 
# combined <-
#   data.frame(value = rbind(
#     social,
#     isolation,
#     support,
#     contact,
#     gathering,
#     disturbance
#   ))
# combined_transpose <- data.frame(transpose(combined))
# rownames(combined_transpose) <- colnames(combined)
# colnames(combined_transpose) <- rownames(combined)
# 
# combined_transpose



