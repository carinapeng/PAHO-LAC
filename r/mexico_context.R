#----------------------------------------#
# COVID-19 - Risk assessment tool - MEX  #
# Margherita Ghiselli, PhD MPH           #
# CDC Liaison officer at PAHO            #
# Email: mghiselli@cdc.gov               #
# Email: ghisellmar@paho.org             #
# Created: 01 Jul 2020                   #
# Revised: 25 Aug 2020                   #
#----------------------------------------#


#---------------------------------#
# INDICATORS AVAILABLE FOR MEXICO #
#---------------------------------#

# CONTEXTO
# contexto01 -> Population density
# contexto02 -> Essential worker
# contexto03 -> Informal worker
# contexto04 -> Internet connection
# contexto05 -> Unemployment benefits
# contexto06 -> Health insurance
# contexto07 -> Co-morbidities
# contexto08 -> Unemployment
# contexto09 -> Income
# contexto10 -> HS diploma 
# contexto11 -> Age 65+
# contexto12 -> Age <17
# contexto13 -> Disability
# contexto14 -> Single parent household
# contexto15 -> Ethnic minority
# contexto16 -> Multi-unit housing
# contexto17 -> Crowded household
# contexto18 -> Vehicle availability
# contexto19 -> Group quarters
# contexto20 XX Households that depend on school feeding programs
# contexto21 -> Households that lives in informal settings
# contexto22 -> Households that is made up of 1 person age 65 or older
# contexto23 XX Presence of 1 or more markets


setwd("Users/carinapeng/PAHO-LAC")

library(lubridate)
library(data.table)
library(ggplot2)
library(ggthemes)
library(reshape)
library(reshape2)
library(tidyr)
library(tidyverse)
library(scales)
library(dplyr)
library(pyramid)
library(XML)
library(plyr)
library(foreign)
library(ggpol)
library(lubridate)
library(factoextra)
library(compare)
library(haven)

# To import the names of the municipalities with their accents: File -> Save with enconding -> UTF-8 
# The variable that contains the result for each indicator is assigned a name that corresponds to its location on the tool. 
# For example: The variable "contexto13" repors the result for indicator #13 on the tab "Contexto" of the risk assessment tool


#------------#
# POPULATION #
#------------#

# contexto01 -> Population density
poblacion <- read.csv("data/raw/mexico/poblacion.csv", fileEncoding="latin1")

poblacion$contexto01 <- 0
poblacion$contexto01[poblacion$densidad <= 0.90] <- 0
poblacion$contexto01[poblacion$densidad > 0.90 & poblacion$densidad <= 2.45] <- 1
poblacion$contexto01[poblacion$densidad > 2.45 & poblacion$densidad <= 8.08] <- 2
poblacion$contexto01[poblacion$densidad > 8.08 & poblacion$densidad <= 22.5] <- 3
poblacion$contexto01[poblacion$densidad > 22.5] <- 4

municipio <- poblacion[c(1,2,3,4)]
municipio$comuna_nome = trimws(municipio$comuna_nome)


#------------------------------#
# PERSON-LEVEL CHARACTERISTICS #
#------------------------------#

# Data retrieved from: https://www.inegi.org.mx/programas/intercensal/2015/default.html#Microdatos
library(readr)
cdmx <- read.csv("data/raw/mexico/census_mexicocity/TR_PERSONA15.CSV", fileEncoding="latin1")
guadalajara <- read.csv("data/raw/mexico/census_jalisco/TR_PERSONA14.CSV", fileEncoding="latin1")
monterrey <- read.csv("data/raw/mexico/census_neuvoleon/TR_PERSONA19.CSV", fileEncoding="latin1")

# Append the datasets of the three states to create a single dataset
cdmx <- rbind(cdmx, guadalajara)
cdmx <- rbind(cdmx, monterrey)

# FORMAT
# Count the number of persons in the dataset
cdmx$id <- 1

# Change the names of variables
names(cdmx)[names(cdmx)=="ENT"] <- "region"
names(cdmx)[names(cdmx)=="MUN"] <- "comuna"
names(cdmx)[names(cdmx)=="SEXO"] <- "sexo"
names(cdmx)[names(cdmx)=="EDAD"] <- "edad"
names(cdmx)[names(cdmx)=="NIVACAD"] <- "edattain"
names(cdmx)[names(cdmx)=="HLENGUA"] <- "lengua"
names(cdmx)[names(cdmx)=="CONACT"] <- "empleo"
names(cdmx)[names(cdmx)=="ocup1_C"] <- "occ"
names(cdmx)[names(cdmx)=="MED_TRASLADO_TRAB1"] <- "transp_work"
names(cdmx)[names(cdmx)=="PERTE_INDIGENA"] <- "indigenous"
names(cdmx)[names(cdmx)=="AFRODES"] <- "afrodes"


# ANALYZE
# contexto11/contexto12 -> Age >= 65 or age <= 17
cdmx$edadcat <- "Unknown"
cdmx$edadcat[cdmx$edad <= 17] <- "0-17"
cdmx$edadcat[cdmx$edad >= 18 & cdmx$edad <=64] <- "18-64"
cdmx$edadcat[cdmx$edad >= 65] <- "65 or older"

edad65 <- cdmx[c(3,5,87,88)]
edad65 <- dcast(edad65, comuna+region ~ edadcat, value.var="id")
edad65$pop <- rowSums(edad65[3:5])

edad65$u17 <- edad65$`0-17` / edad65$pop
edad65$o65 <- edad65$`65 or older` / edad65$pop

edad65$u1790 <- quantile(edad65$u17, c(.90)) 
edad65$contexto12[edad65$u17 >= edad65$u1790] <- 1
edad65$contexto12[is.na(edad65$contexto12)] = 0

edad65$o6590 <- quantile(edad65$o65, c(.90)) 
edad65$contexto11[edad65$o65 >= edad65$o6590] <- 1
edad65$contexto11[is.na(edad65$contexto11)] = 0


# contexto10 -> Educational attainment
cdmx$edcat <- 0
cdmx$edcat[cdmx$edattain >= 0 & cdmx$edattain <= 3] <- 1
cdmx$edcat[cdmx$edattain >= 4 & cdmx$edattain <= 14] <- 0

education <- cdmx[c(3,5,87,89)]
education$id <- as.numeric(education$id)
education$edcat <- as.numeric(education$edcat)
education <- aggregate(. ~ comuna+region, education, sum)

education$diploma <- education$edcat / education$id

education$diploma90 <- quantile(education$diploma, c(.90)) 
education$contexto10[education$diploma >= education$diploma90] <- 1
education$contexto10[is.na(education$contexto10)] = 0


# contexto15 -> Ethnic minority
cdmx$indigcat <- 0
cdmx$indigcat[cdmx$indigenous == 1 | cdmx$indigenous == 2 | cdmx$afrodes == 1 | cdmx$afrodes == 2] <- 1

indig <- cdmx[c(3,5,87,90)]
indig$id <- as.numeric(indig$id)
indig$empleocat <- as.numeric(indig$indigcat)
indig <- aggregate(. ~ comuna+region, indig, sum)

indig$ethnic <- indig$indigcat / indig$id

indig$ethnic90 <- quantile(indig$ethnic, c(.90)) 
indig$contexto15[indig$ethnic >= indig$ethnic90] <- 1
indig$contexto15[is.na(indig$contexto15)] = 0 


# contexto08 -> Employment status
cdmx$empleocat <- 0
cdmx$empleocat[cdmx$empleo == 10] <- 1
cdmx$empleocat[cdmx$empleo >= 11 & cdmx$empleo <=99] <- 0

employment <- cdmx[c(3,5,87,91)]
employment$id <- as.numeric(employment$id)
employment$empleocat <- as.numeric(employment$empleocat)
employment <- aggregate(. ~ comuna+region, employment, sum)

employment$employ <- employment$empleocat / employment$id

employment$employ90 <- quantile(employment$employ, c(.90)) 
employment$contexto08[employment$employ >= employment$employ90] <- 1
employment$contexto08[is.na(employment$contexto08)] = 0


# contexto05 -> Transportation to work
# cdmx$trworkcat <- 0
# cdmx$trworkcat[cdmx$transp_work == 1 | cdmx$transp_work == 2 | cdmx$transp_work == 4] <- 1

# transport <- cdmx[c(3,5,87,92)]
# transport$id <- as.numeric(transport$id)
# transport$essential <- as.numeric(transport$trworkcat)
# transport <- aggregate(. ~ comuna+region, transport, sum)

# transport$needed <- transport$essential / transport$id

# transport$needed90 <- quantile(transport$needed, c(.90)) 
# transport$contexto05[transport$needed >= transport$needed90] <- 1
# transport$contexto05[is.na(transport$contexto05)] = 0   


# contexto22 -> Single-occupant household age 65 or older
nbpers65 <- cdmx[c(1,3,5,15)] 
nbpers65$ID_VIV <- as.character(nbpers65$ID_VIV)

nbpers65a <- nbpers65 %>% group_by(ID_VIV) %>% filter(n() == 1)
nbpers65a <- nbpers65a[-c(4)]
nbpers65a$hh <- 1

nbpers65 <- merge(nbpers65, nbpers65a, by=c("comuna","ID_VIV","region"), all=TRUE)
nbpers65$hh[is.na(nbpers65$hh)] = 0

nbpers65$single <- 0
nbpers65$single[nbpers65$hh == 1 & nbpers65$edad >= 65] <- 1

nbpers65 <- nbpers65[-c(2,4,5)]
nbpers65 <- aggregate(. ~ comuna+region, nbpers65, mean)

nbpers65$single90 <- quantile(nbpers65$single, c(.90)) 
nbpers65$contexto22[nbpers65$single >= nbpers65$single90] <- 1
nbpers65$contexto22[is.na(nbpers65$contexto22)] = 0  


# contexto13 -> Disability benefits
disability <- read_dta("ciudad_mexico/censo_cdmx/mx2015_disability.dta")

names(disability)[names(disability)=="disemp"] <- "invalidez"
names(disability)[names(disability)=="geo1_mx2015"] <- "region"
names(disability)[names(disability)=="geo2_mx2015"] <- "comuna"

disability <- subset(disability, disability$region == 9 |    # CDMX
                       disability$region == 14 |   # Jalisco
                       disability$region == 19)    # Nuevo Leon

disability$pers <- 1

disability$yes <- 0
disability$yes[disability$invalidez == 1] <- 1

disability <- disability[c(6,7,11,12)]
disability$id <- as.numeric(disability$pers)
disability$yes <- as.numeric(disability$yes)
disability <- aggregate(. ~ comuna+region, disability, sum)

disability$disab <- disability$yes / disability$pers

disability$disab90 <- quantile(disability$disab, c(.90)) 
disability$contexto13[disability$disab >= disability$disab90] <- 1
disability$contexto13[is.na(disability$contexto13)] = 0 

cdmx <- subset(disability, disability$region == 9)
cdmx$comuna <- sub('.', '', cdmx$comuna)

guadalajara <- subset(disability, disability$region == 14)
guadalajara$comuna = gsub("14", "", guadalajara$comuna)
guadalajara$comuna[guadalajara$comuna == "0"] <- "014"

monterrey <- subset(disability, disability$region == 19)
monterrey$comuna = gsub("19", "", monterrey$comuna)
monterrey$comuna[monterrey$comuna == "0"] <- "019"

disability <- rbind(cdmx, guadalajara)
disability <- rbind(disability, monterrey)  

disability$region <- as.numeric(disability$region)
disability$comuna <- as.numeric(disability$comuna)



#------------#
# OCCUPATION #
#------------#

# For Q1 2020 data, see Cuestionario de Ocupación y Empleo (COE): https://www.inegi.org.mx/programas/enoe/15ymas/default.html#Microdatos
# For categories, see: http://www3.inegi.org.mx/rnm/index.php/catalog/214/datafile/F18/V1273
library(readr)
ocup1 <- read.csv("data/raw/mexico/occupation/conjunto_de_datos_coe1_enoe_2020_1t/conjunto_de_datos/conjunto_de_datos_coe1_enoe_2020_1t.csv")
ocup2 <- read.csv("data/raw/mexico/occupation/conjunto_de_datos_coe2_enoe_2020_1t/conjunto_de_datos/conjunto_de_datos_coe2_enoe_2020_1t.csv")
demog <- read.csv("data/raw/mexico/occupation/conjunto_de_datos_sdem_enoe_2020_1t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2020_1t.csv")


# FORMAT
# Add the geographical variables to the COE datasets
names(ocup2)[names(ocup2)=="ï..cd_a"] <- "cd_a"

# Create linking variable
ocup1$key <- paste(ocup1$cd_a, ocup1$ent, ocup1$con, ocup1$v_sel, ocup1$h_hog, ocup1$h_mud, ocup1$n_ren)
ocup2$key <- paste(ocup2$cd_a, ocup2$ent, ocup2$con, ocup2$v_sel, ocup2$h_hog, ocup2$h_mud, ocup2$n_ren)
demog$key <- paste(demog$cd_a, demog$ent, demog$con, demog$v_sel, demog$h_hog, demog$h_mud, demog$n_ren)

# Change the names of variables
names(ocup1)[names(ocup1)=="ent"] <- "region"
names(ocup2)[names(ocup2)=="ent"] <- "region"
names(demog)[names(demog)=="ent"] <- "region"
names(demog)[names(demog)=="mun"] <- "comuna"

# Keep essential variables
ocup1 <- ocup1[c(3,84,93,96,99,117,120,187)]
ocup2 <- ocup2[c(2,34,126)]
demog <- demog[c(3,9,105)]

# Keep departamentos for the city of CDMX (09), Guadalajara (14) and Monterrey (19)
ocup1 <- subset(ocup1, ocup1$region == 9 | ocup1$region == 14 | ocup1$region == 19)
ocup2 <- subset(ocup2, ocup2$region == 9 | ocup2$region == 14 | ocup2$region == 19)
demog <- subset(demog, demog$region == 9 | demog$region == 14 | demog$region == 19)

# Merge datasets
total <- merge(ocup2, ocup1, by="key", all=TRUE) 
total <- merge(total, demog, by="key", all=TRUE)  
total <- total[!duplicated(total), ]                                         
total <- total[-c(1,2,4)]

# Count the number of persons in the dataset
total$pers <- 1


# ANALYZE
# Codes retrieved from: https://www.inegi.org.mx/app/scian/
# contexto02 -> Occupation: Essential workers
total$essential <- 0
total$essential[(total$p4a >= 1100 & total$p4a <= 1199) |      # Agricultura, cría y explotación de animales, aprovechamiento forestal, pesca y caza
                  (total$p4a >= 2100 & total$p4a <= 2199) |      # Minería
                  (total$p4a >= 2200 & total$p4a <= 2299) |      # Generación, transmisión, distribución y comercialización de energía eléctrica, suministro de agua y de gas natural por ductos al consumidor final
                  (total$p4a >= 4800 & total$p4a <= 4999) |      # Transportes, correos y almacenamiento
                  (total$p4a >= 5100 & total$p4a <= 5199) |      # Información en medios masivos
                  (total$p4a >= 6200 & total$p4a <= 6299) |      # Servicios de salud y de asistencia social
                  (total$p4a >= 7200 & total$p4a <= 7299) |      # Servicios de alojamiento temporal y de preparación de alimentos y bebidas
                  (total$p4a >= 9300 & total$p4a <= 9399)] <- 1  # Actividades legislativas, gubernamentales, de impartición de justicia y de organismos internacionales y extraterritoriales

essential <- total[c(8,9,10,11)]
essential$id <- as.numeric(essential$pers)
essential$essential <- as.numeric(essential$essential)
essential <- aggregate(. ~ comuna+region, essential, sum)

essential$needed <- essential$essential / essential$pers

essential$contexto02[essential$needed <= 0.1] <- 0
essential$contexto02[essential$needed > 0.1 & essential$needed < 0.31] <- 1
essential$contexto02[essential$needed >= 0.31] <- 2


# contexto03 -> Occupation: Informal sector (defined as no contract and independently owned)
total$informal <- 0
total$informal[(total$p3j == 2 | total$p3j == 19) &      # En su empleo, cuenta con un contrato por escrito
                 (total$p4c == 1)] <- 1                   # Esto negocio es de tipo independiente, personal o familiar   

informal <- total[c(8,9,10,12)]
informal$pers <- as.numeric(informal$pers)
informal$informal <- as.numeric(informal$informal)
informal <- aggregate(. ~ comuna+region, informal, sum)

informal$needed <- informal$informal / informal$pers

informal$contexto03[informal$needed <= 0.1] <- 0
informal$contexto03[informal$needed > 0.1 & informal$needed < 0.31] <- 1
informal$contexto03[informal$needed >= 0.31] <- 2       


# contexto05 -> Unemployment benefits
total$unemployment <- 0
total$unemployment[(total$p3m1 == 1) | (total$p3m4 == 1) | (total$p3m7 == 1)] <- 1

unemployment <- total[c(8,9,10,13)]
unemployment$pers <- as.numeric(unemployment$pers)
unemployment$unemployment <- as.numeric(unemployment$unemployment)
unemployment <- aggregate(. ~ comuna+region, unemployment, sum)

unemployment$safety <- unemployment$unemployment / unemployment$pers

unemployment$contexto05[unemployment$safety <= 0.1] <- 0
unemployment$contexto05[unemployment$safety > 0.1 & unemployment$safety < 0.31] <- 1
unemployment$contexto05[unemployment$safety >= 0.31] <- 2 


# contexto06 -> Health insurance
total$healthinsure <- 0
total$healthinsure[(total$p3m1 == 6) | (total$p6d < 6)] <- 1

healthinsure <- total[c(8,9,10,14)]
healthinsure$pers <- as.numeric(healthinsure$pers)
healthinsure$healthinsure <- as.numeric(healthinsure$healthinsure)
healthinsure <- aggregate(. ~ comuna+region, healthinsure, sum)

healthinsure$insure <- healthinsure$healthinsure / healthinsure$pers

healthinsure$contexto06[healthinsure$insure <= 0.1] <- 0
healthinsure$contexto06[healthinsure$insure > 0.1 & healthinsure$insure < 0.31] <- 1
healthinsure$contexto06[healthinsure$insure >= 0.31] <- 2 



#---------------------------------#
# HOUSEHOLD-LEVEL CHARACTERISTICS #
#---------------------------------#

# Data retrieved from: https://www.inegi.org.mx/programas/intercensal/2015/default.html#Microdatos
library(readr)
vivienda <- read.csv("data/raw/mexico/census_mexicocity/TR_VIVIENDA15.CSV")
guadalajara <- read.csv("data/raw/mexico/census_jalisco/TR_VIVIENDA14.CSV")
monterrey <- read.csv("data/raw/mexico/census_neuvoleon/TR_VIVIENDA19.CSV")

# Append the datasets of the three states to create a single dataset
vivienda <- rbind(vivienda, guadalajara)
vivienda <- rbind(vivienda, monterrey)


# FORMAT
# Count the number of households in the dataset
vivienda$hh <- 1

# Change the names of variables
names(vivienda)[names(vivienda)=="ENT"] <- "region"
names(vivienda)[names(vivienda)=="MUN"] <- "comuna"
names(vivienda)[names(vivienda)=="AGUA_ENTUBADA"] <- "water"  
names(vivienda)[names(vivienda)=="TIPOHOG"] <- "hhtype" 
names(vivienda)[names(vivienda)=="INGTRHOG"] <- "income" 
names(vivienda)[names(vivienda)=="AUTOPROP"] <- "auto"
names(vivienda)[names(vivienda)=="JEFE_SEXO"] <- "hhhsex"
names(vivienda)[names(vivienda)=="NUMPERS"] <- "nbpersons"
names(vivienda)[names(vivienda)=="PISOS"] <- "piso"
names(vivienda)[names(vivienda)=="PAREDES"] <- "parede"
names(vivienda)[names(vivienda)=="INTERNET"] <- "internet"



# ANALYZE
# Piped water
# vivienda$watercat <- 0
# vivienda$watercat[vivienda$water >= 1 & vivienda$water <=2] <- 1

# water <- vivienda[c(2,4,89,90)]
# water$hh <- as.numeric(water$hh)
# water$watercat <- as.numeric(water$watercat)
# water <- aggregate(. ~ comuna+region, water, sum)

# water$piped <- water$watercat / water$hh

# water$piped90 <- quantile(water$piped, c(.90)) 
# water$contexto02[water$piped >= water$piped90] <- 1
# water$contexto02[is.na(water$contexto02)] = 0


# contexto17 -> Number of persons in the HH
nbpers <- vivienda[c(2,4,64,89)]
nbpers <- aggregate(. ~ comuna+region, nbpers, sum)

nbpers$avg <- nbpers$nbperson / nbpers$hh

nbpers$contexto17 <- 0
nbpers$contexto17[nbpers$avg > 3] <- 1


# contexto19 -> Group quarters
vivienda$gqcat <- 0
vivienda$gqcat[vivienda$hhtype == 5] <- 1

groupq <- vivienda[c(2,4,89,90)]
groupq$hh <- as.numeric(groupq$hh)
groupq$gqcat <- as.numeric(groupq$gqcat)
groupq <- aggregate(. ~ comuna+region, groupq, sum)

groupq$gq <- groupq$gqcat / groupq$hh

groupq$gq90 <- quantile(groupq$gq, c(.90)) 
groupq$contexto19[groupq$gq >= groupq$gq90] <- 1
groupq$contexto19[is.na(groupq$contexto19)] = 0


# contexto16 -> Multi-unit dwellings
vivienda$multicat <- 0
vivienda$multicat[vivienda$hhtype >= 3 & vivienda$hhtype <= 9] <- 1

multihouse <- vivienda[c(2,4,89,91)]
multihouse$hh <- as.numeric(multihouse$hh)
multihouse$multicat <- as.numeric(multihouse$multicat)
multihouse <- aggregate(. ~ comuna+region, multihouse, sum)

multihouse$multi <- multihouse$multicat / multihouse$hh

multihouse$multi90 <- quantile(multihouse$multi, c(.90)) 
multihouse$contexto16[multihouse$multi >= multihouse$multi90] <- 1
multihouse$contexto16[is.na(multihouse$contexto16)] = 0


# contexto09 -> HH income
vivienda$income[is.na(vivienda$income)] = 0
vivienda$income[vivienda$income == 1999999] <- 0

avgincome <- vivienda[c(2,4,88)]
avgincome <- avgincome <- aggregate(. ~ comuna+region, avgincome, mean)

avgincome$income90 <- quantile(avgincome$income, c(.90)) 
avgincome$contexto09[avgincome$income >= avgincome$income90] <- 1
avgincome$contexto09[is.na(avgincome$contexto09)] = 0


# contexto18 -> Vehicle ownership
vivienda$autocat <- 0
vivienda$autocat[vivienda$auto == 7] <- 1

autoown <- vivienda[c(2,4,89,92)]
autoown$hh <- as.numeric(autoown$hh)
autoown$autocat <- as.numeric(autoown$autocat)
autoown <- aggregate(. ~ comuna+region, autoown, sum)

autoown$multi <- autoown$autocat / autoown$hh

autoown$multi90 <- quantile(autoown$multi, c(.90)) 
autoown$contexto18[autoown$multi >= autoown$multi90] <- 1
autoown$contexto18[is.na(autoown$contexto18)] = 0


# contexto14 -> Sex of HHH
vivienda$hhhsexcat <- 0
vivienda$hhhsexcat[vivienda$hhhsex == 3] <- 1

sexhhh <- vivienda[c(2,4,89,93)]
sexhhh$hh <- as.numeric(sexhhh$hh)
sexhhh$hhhsexcat <- as.numeric(sexhhh$hhhsexcat)
sexhhh <- aggregate(. ~ comuna+region, sexhhh, sum)

sexhhh$multi <- sexhhh$hhhsexcat / sexhhh$hh

sexhhh$multi90 <- quantile(sexhhh$multi, c(.90)) 
sexhhh$contexto14[sexhhh$multi >= sexhhh$multi90] <- 1
sexhhh$contexto14[is.na(sexhhh$contexto14)] = 0


# contexto21 -> Household located in an informal setting
vivienda$pisocat <- 0
vivienda$pisocat[vivienda$piso == 1 & (vivienda$parede <= 3)] <- 1

informal <- vivienda[c(2,4,89,94)]
informal$hh <- as.numeric(informal$hh)
informal$pisocat <- as.numeric(informal$pisocat)
informal <- aggregate(. ~ comuna+region, informal, sum)

informal$multi <- informal$pisocat / informal$hh

informal$multi90 <- quantile(informal$multi, c(.90)) 
informal$contexto21[informal$multi >= informal$multi90] <- 1
informal$contexto21[is.na(informal$contexto21)] = 0


# contexto04 -> Internet connection
vivienda$internetcat <- 0
vivienda$internetcat[vivienda$internet == 5] <- 1

internet <- vivienda[c(2,4,89,95)]
internet$hh <- as.numeric(internet$hh)
internet$internetcat <- as.numeric(internet$internetcat)
internet <- aggregate(. ~ comuna+region, internet, sum)

internet$multi <- internet$internetcat / internet$hh

internet$multi90 <- quantile(internet$multi, c(.90)) 
internet$contexto04[internet$multi >= internet$multi90] <- 1
internet$contexto04[is.na(internet$contexto04)] = 0


# contexto07 -> Co-morbidities


# contexto23 -> Presence of 1+ markets in the neighborhood


# contexto20 -> Households that depend on school feeding programs

autoown <- autoown[c(1,2,7)]              # OK
avgincome <- avgincome[c(1,2,5)]          # OK
disability <- disability[c(1,2,8)]        # OK
edad65 <- edad65[c(1,2,10,12)]            # OK
education <- education[c(1,2,7)]          # OK
employment <- employment[c(1,2,7)]        # OK
essential <- essential[c(1,2,7)]          # OK
groupq <- groupq[c(1,2,7)]                # OK
healthinsure <- healthinsure[c(1,2,6)]    # OK
indig <- indig[c(1,2,8)]                  # OK
informal <- informal[c(1,2,7)]            # OK
internet <- internet[c(1,2,7)]            # OK
multihouse <- multihouse[c(1,2,7)]        # OK
nbpers <- nbpers[c(1,2,6)]                # OK
nbpers65 <- nbpers65[c(1,2,5)]            # OK
poblacion <- poblacion[c(1,2,3,4)]        # OK
sexhhh <- sexhhh[c(1,2,7)]                # OK
unemployment <- unemployment[c(1,2,6)]    # OK

# Merge all results into a single dataset, stratified by municipality (comuna)
# Need to add disability in
results <- merge(autoown, avgincome, by=c("comuna","region"), all=TRUE)
results <- merge(results, edad65, by=c("comuna","region"), all=TRUE)
results <- merge(results, education, by=c("comuna","region"), all=TRUE)
results <- merge(results, employment, by=c("comuna","region"), all=TRUE)
results <- merge(results, essential, by=c("comuna","region"), all=TRUE)
results <- merge(results, groupq, by=c("comuna","region"), all=TRUE)
results <- merge(results, healthinsure, by=c("comuna","region"), all=TRUE)
results <- merge(results, indig, by=c("comuna","region"), all=TRUE)
results <- merge(results, informal, by=c("comuna","region"), all=TRUE)
results <- merge(results, internet, by=c("comuna","region"), all=TRUE)
results <- merge(results, multihouse, by=c("comuna","region"), all=TRUE)
results <- merge(results, nbpers, by=c("comuna","region"), all=TRUE)
results <- merge(results, nbpers65, by=c("comuna","region"), all=TRUE)
results <- merge(results, poblacion, by=c("comuna","region"), all=TRUE)
results <- merge(results, sexhhh, by=c("comuna","region"), all=TRUE)
results <- merge(results, unemployment, by=c("comuna","region"), all=TRUE)

results <- unique(results)
results <- results[-c(1, 2)]
names(results)[names(results) == "estado"] <- "region"
names(results)[names(results) == "comuna_nome"] <- "comuna"
results <- na.omit(results)

# Reorder columns
results <-
  results[c(
    "region",
    "comuna",
    "contexto02",
    "contexto04",
    "contexto05",
    "contexto06",
    "contexto08",
    "contexto09",
    "contexto10",
    "contexto11",
    "contexto12",
    "contexto14",
    "contexto15",
    "contexto16",
    "contexto17",
    "contexto18",
    "contexto19",
    "contexto21",
    "contexto22"
  )]

saveRDS(results, "data/mexico_census_analyzed.rds")
