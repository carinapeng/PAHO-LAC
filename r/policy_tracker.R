


library(readxl)

setwd("Users/carinapeng/PAHO-LAC")

url <- "https://www.who.int/docs/default-source/documents/phsm/20200826-phms-who-int.zip?sfvrsn=caa32ba8_2"
# destfile <- "data/raw/policy_tracker.xlsx"
# download.file(url, destfile)


# /Users/carinapeng/Downloads/20200826-phms-who-int/WHO_PHSM_Cleaned_V1_20_08_26.xlsx

temp <- tempfile()
download.file(url,temp)
data <- read.table(unz(temp, ".xlsx"))
unlink(temp)

temp <- tempfile()
temp2 <- tempfile()

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
data <- read_xls(file.path(temp2))

unlink(c(temp, temp2))