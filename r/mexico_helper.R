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

# EPIDEMIOLOGIA
# epidemiologia01 -> Incidence rate per 100,000
# epidemiologia02 -> Incidence rate per 100,000 (men)
# epidemiologia03 -> Incidence rate per 100,000 (65 or older)
# epidemiologia04 XX Percent positivity rate
# epidemiologia05 XX Doubling rate
# epidemiologia06 XX Neighborhood-specific reproductive rate (Rt)
# epidemiologia07 -> Presence of COVID-19 hotspot in an adjacent neighborhood
# epidemiologia08 -> Case mortality rate
# epidemiologia09 -> Case mortality rate (men)
# epidemiologia10 -> Case mortality rate (65 or older)
# epidemiologia11 XX Number of excess deaths compared to 2015-2019

# MEDIDAS DE MITIGACION
# mitigacion01 -> Percent positivity rate ≥5% & Testing rate <1/100,000
# mitigacion02 -> Percent tests with results within 72 hours
# mitigacion03 -> Prop. HCW infected with COVID-19
# mitigacion04 -> Prop. confirmed or probable cases hospitalized
# mitigacion05 -> Prop. hospital beds occupied  by COVID-19 patient
# mitigacion06 -> Prop. ICU beds occupied by COVID-19 patients
# mitigacion07 -> Prop. confirmed & hospitalized cases admitted <48h
# mitigacion08 -> Prop. confirmed & hospitalized cases admitted <48h, + ICU
# mitigacion09 -> Prop. confirmed & hospitalized cases admitted <48h, + ER
# mitigacion10 -> Prop. newborns who were born in a hospital during the last month
# mitigacion11 -> Physical and social distancing and movement measures in place 
# mitigacion12 -> Implementation of self-isolation and forced isolation measures
# mitigacion13 -> Implementation of support measures during self-isolation/quarantine
# mitigacion14 -> Prop. new COVID-19 cases who been placed in quarantine
# mitigacion15 -> Prop. new COVID-19 cases who were self-isolating before the diagnosis
# mitigacion16 -> Implementation of contact tracing
# mitigacion17 -> Prop. new COVID-19 cases identified from a contact list
# mitigacion18 -> Nb. persons tested for every new COVID-19 case detected, in last 7 days
# mitigacion19 -> Neighborhood held 1+ mass gatherings in the last 30 days
# mitigacion20 -> Neighborhood experienced social disturbances in the last 30 days
# mitigacion21 -> Prop. population who adheres to mitigation measures - work/school
# mitigacion22 -> Prop. population who adheres to mitigation measures - recreational activities
# mitigacion23 -> Prop. population who takes public transportation





#----------------------------# 
# EPIDEMIOLOGICAL INDICATORS #
#----------------------------#

# Data retrieved from: https://coronavirus.gob.mx/datos/#DownZCSV
library(readr)
covid <- read.csv("data/raw/mexico/incidence/Casos_Diarios_Municipio_Confirmados_20200701.csv", encoding = "UTF-8")


# FORMAT
# Change the names of variables
names(covid)[names(covid)=="cve_ent"] <- "comuna"

# Keep only the information from CDMX, GUadalajara and Monterrey 
covid <- subset(covid, (covid$comuna >=  9000 & covid$comuna <=  9999) |   # Ciudad de Mexico
                       (covid$comuna >= 14000 & covid$comuna <= 14999) |   # Guadalajara
                       (covid$comuna >= 19000 & covid$comuna <= 19999))    # Monterrey

# Generate "region" variable
covid$region <- 0
covid$region[covid$comuna >=  9000 & covid$comuna <=  9999] <- 9
covid$region[covid$comuna >= 14000 & covid$comuna <= 14999] <- 14
covid$region[covid$comuna >= 19000 & covid$comuna <= 19999] <- 19

covid$comuna <- as.character(covid$comuna)

cdmx_covid <- subset(covid, covid$region == 9)
cdmx_covid$comuna <- sub('.', '', cdmx_covid$comuna)

guadalajara_covid <- subset(covid, covid$region == 14)
  guadalajara_covid$comuna <- substr(guadalajara_covid$comuna, 3, 5)  

monterrey_covid <- subset(covid, covid$region == 19)
  monterrey_covid$comuna <- substr(monterrey_covid$comuna, 3, 5) 

covid <- rbind(cdmx_covid, guadalajara_covid)
covid <- rbind(covid, monterrey_covid) 


# ANALYZE
# epidemiologia01 -> Incidence rate (7-day)
covid$cases_tot <- rowSums(covid[4:175])
covid$cases_7day <- rowSums(covid[168:175])

covid$incidence_7day <- (covid$cases_7day / covid$poblacion) * 100000
covid$epidemiologia01[covid$incidence_7day >= 1] <- 1
covid$epidemiologia01[covid$incidence_7day < 1] <- 0


# epidemiologia07 -> Neighboring municipality is affected
covid$epidemiologia07 <- 1

covid$region <- as.numeric(covid$region)
covid$comuna <- as.numeric(covid$comuna)


#---------------------# 
# HEALTHCARE SERVICES # 
#---------------------#

# SINAVE data retrieved from: https://datos.cdmx.gob.mx/explore/dataset/base-covid-sinave/table/
library(readr)
surv <- read.csv("data/raw/mexico/surveillance/base-covid-sinave.csv")


# FORMAT
# Count the number of persons in the dataset
surv$case <- 1

# Change the names of variables
names(surv)[names(surv)=="cve_entidad_residencia"] <- "region"
names(surv)[names(surv)=="municipio_residencia"] <- "comuna"
names(surv)[names(surv)=="sexo"] <- "sexo"
names(surv)[names(surv)=="fecha_ingreso"] <- "fecha_de_registro"
names(surv)[names(surv)=="edad"] <- "edad"
names(surv)[names(surv)=="resultado_definitivo"] <- "outcome"

# Keep only the information from CDMX, Guadalajara and Monterrey
surv <- subset(surv, surv$region == 9 | region == 14 | region == 19)

# Drop non-existing comunas
surv <- subset(surv, !(comuna == 999 & region == 9))
surv <- subset(surv, !(comuna == 106 & region == 9))
surv <- subset(surv, !(comuna == 66 & region == 19))

# Create a variable that records today's date
surv$today <- Sys.Date()
surv$fecha_defuncion[surv$fecha_defuncion == "9999-99-99"] <- NA
  surv$fecha_defuncion <- as.Date(surv$fecha_defuncion, format="%Y-%m-%d")


# ANALYZE
# epidemiologia02 -> Incidence rate in the last 7 days among men
surv$casem <- 0
surv$casem[surv$sexo == 1 & (surv$today >= surv$fecha_de_registro & surv$fecha_de_registro <= (surv$today - 7))] <- 1
surv$tot_sem <- 0
surv$tot_sem[surv$today >= surv$fecha_de_registro & surv$fecha_de_registro <= (surv$today - 7)] <- 1

incidencem <- surv %>%
  select(region, comuna, casem, tot_sem)
incidencem$tot_sem <- as.numeric(incidencem$tot_sem)
incidencem$casem <- as.numeric(incidencem$casem)
incidencem <- aggregate(. ~ comuna+region, incidencem, sum)

incidencem$incidencem <- (incidencem$casem / incidencem$tot_sem) * 100

incidencem$epidemiologia02[incidencem$incidencem >= 50] <- 1
incidencem$epidemiologia02[incidencem$incidencem < 50] <- 0


# epidemiologia03 -> Incidence rate in the last 7 days among persons age 65 or older
surv$case65 <- 0
surv$case65[surv$edad >= 65 & (surv$today >= surv$fecha_de_registro & surv$fecha_de_registro <= (surv$today - 7))] <- 1

incidence65 <- surv[c(8,9,39,40)]
incidence65$tot_sem <- as.numeric(incidence65$tot_sem)
incidence65$case65 <- as.numeric(incidence65$case65)
incidence65 <- aggregate(. ~ comuna+region, incidence65, sum)

incidence65$incidence65 <- (incidence65$case65 / incidence65$tot_sem) * 100

incidence65$epidemiologia03[incidence65$incidence65 >= 30] <- 1
incidence65$epidemiologia03[incidence65$incidence65 < 30] <- 0


# epidemiologia09 -> Mortality rate in the last 7 days
surv$dead <- 0
surv$dead[surv$today >= surv$fecha_defuncion & surv$fecha_defuncion <= (surv$today - 7)] <- 1

death <- surv[c(8,9,39,41)]
death$tot_sem <- as.numeric(death$tot_sem)
death$dead <- as.numeric(death$dead)
death <- aggregate(. ~ comuna+region, death, sum)

death$mortality_7day <- (death$dead / death$tot_sem) * 100000

death$epidemiologia09[death$mortality_7day >= 26] <- 1
death$epidemiologia09[death$mortality_7day < 26] <- 0


# epidemiologia10 -> Mortality rate in the last 7 days among men
surv$deadm <- 0
surv$deadm[surv$sexo == 1 & (surv$today >= surv$fecha_defuncion & surv$fecha_defuncion <= (surv$today - 7))] <- 1

deathm <- surv[c(8,9,39,42)]
deathm$dead <- as.numeric(deathm$dead)
deathm$deadm <- as.numeric(deathm$deadm)
deathm <- aggregate(. ~ comuna+region, deathm, sum)

deathm$mortality_7day <- (deathm$deadm / deathm$dead) * 100

deathm$epidemiologia10[deathm$mortality_7day >= 9] <- 1
deathm$epidemiologia10[deathm$mortality_7day < 9] <- 0


# epidemiologia11 -> Mortality rate in the last 7 days among persons age 65 or older
surv$dead65 <- 0
surv$dead65[surv$edad >= 65 & (surv$today >= surv$fecha_defuncion & surv$fecha_defuncion <= (surv$today - 7))] <- 1

death65 <- surv[c(8,9,39,43)]
death65$dead <- as.numeric(death65$dead)
death65$dead65 <- as.numeric(death65$dead65)
death65 <- aggregate(. ~ comuna+region, death65, sum)

death65$mortality_7day <- (death65$dead65 / death65$dead) * 100

death65$epidemiologia11[death65$mortality_7day >= 9] <- 1
death65$epidemiologia11[death65$mortality_7day < 9] <- 0


# mitigacion03 -> Infected HCWs
# surv$hcw <- 0
# surv$hcw[surv$ocup1 == "MEDICOS" | surv$ocup1 == "ENFERMERAS" | surv$hcw == "OTROS TRABAJADORES DE LA SALUD" | surv$hcw == "DENTISTAS" | surv$hcw == "LABORATORISTAS"] <- 1

# hcw <- surv[c(8,9,44)]
# hcw$tot_sem <- as.numeric(hcw$tot_sem)
# hcw$hcw <- as.numeric(hcw$hcw)
# hcw <- aggregate(. ~ comuna+region, hcw, sum)

# hcw$hcwsick <- (hcw$hcw / hcw$tot_sem) * 100

# hcw$mitigacion03[hcw$hcwsick >= 20] <- 1
# hcw$mitigacion03[hcw$hcwsick < 20] <- 0


# mitigacion04 -> Cases hospitalized
surv$hospitalized <- 0
surv$hospitalized[surv$TIPO_PACIENTE == 2] <- 1

hospital <- surv[c(8,9,39,44)]
hospital$tot_sem <- as.numeric(hospital$tot_sem)
hospital$hospitalized <- as.numeric(hospital$hospitalized)
hospital <- aggregate(. ~ comuna+region, hospital, sum)

hospital$hospitalsick <- (hospital$hospitalized / hospital$tot_sem) * 100

hospital$mitigacion04[hospital$hospitalsick >= 20] <- 1
hospital$mitigacion04[hospital$hospitalsick < 20] <- 0


# mitigacion06 -> Cases transferred to the ICU
surv$icu <- 0
surv$icu[surv$UCI == 1] <- 1

icu <- surv[c(8,9,39,45)]
icu$tot_sem <- as.numeric(icu$tot_sem)
icu$icu <- as.numeric(icu$icu)
icu <- aggregate(. ~ comuna+region, icu, sum)

icu$icusick <- (icu$icu / icu$tot_sem) *100

icu$mitigacion06[icu$icusick >= 11.5] <- 1
icu$mitigacion06[icu$icusick < 11.5] <- 0


# mitigacion18 -> Proportion of persons tested for every new COVID-19 case detected, in the last week
surv$confirmed <- 0
surv$confirmed[surv$outcome == 1 & (surv$today >= surv$fecha_de_registro & surv$fecha_de_registro <= (surv$today - 7))] <- 1

confirmed <- surv[c(8,9,39,46)]
confirmed$tot_sem <- as.numeric(confirmed$tot_sem)
confirmed$confirmed <- as.numeric(confirmed$confirmed)
confirmed <- aggregate(. ~ comuna+region, confirmed, sum)

confirmed$confirmedsick <- (confirmed$tot_sem / confirmed$confirmed) * 100

confirmed$mitigacion18[confirmed$confirmedsick >= 50] <- 1
confirmed$mitigacion18[confirmed$confirmedsick < 50] <- 0



#---------# 
# RESULTS #
#---------#
  
# Remove unnecesary datasets
rm("cdmx", "guadalajara", "monterrey", "demog", "nbpers65a", "ocup1", "ocup2", "surv", "total", "vivienda")  
  
# From each dataset, keep only the results for the risk assessment tool
autoown <- autoown[c(1,2,7)]              # OK
avgincome <- avgincome[c(1,2,5)]          # OK
confirmed <- confirmed[c(1,2,6)]          # OK
covid <- covid[c(1,214,218,219)]          # OK
death <- death[c(1,2,6)]                  # OK
death65 <- death65[c(1,2,7)]              # OK
deathm <- deathm[c(1,2,7)]                # OK
disability <- disability[c(1,2,8)]        # OK
edad65 <- edad65[c(1,2,10,12)]            # OK
education <- education[c(1,2,7)]          # OK
employment <- employment[c(1,2,7)]        # OK
essential <- essential[c(1,2,7)]          # OK
groupq <- groupq[c(1,2,7)]                # OK
healthinsure <- healthinsure[c(1,2,6)]    # OK
hospital <- hospital[c(1,2,6)]            # OK
icu <- icu[c(1,2,6)]                      # OK
incidence65 <- incidence65[c(1,2,6)]      # OK
incidencem <- incidencem[c(1,2,6)]        # OK
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
results <- merge(autoown, avgincome, by=c("comuna","region"), all=TRUE)
results <- merge(results, confirmed, by=c("comuna","region"), all=TRUE)
results <- merge(results, covid, by=c("comuna","region"), all=TRUE)
results <- merge(results, death, by=c("comuna","region"), all=TRUE)
results <- merge(results, death65, by=c("comuna","region"), all=TRUE)
results <- merge(results, deathm, by=c("comuna","region"), all=TRUE)
results <- merge(results, disability, by=c("comuna","region"), all=TRUE)
results <- merge(results, edad65, by=c("comuna","region"), all=TRUE)
results <- merge(results, education, by=c("comuna","region"), all=TRUE)
results <- merge(results, employment, by=c("comuna","region"), all=TRUE)
results <- merge(results, essential, by=c("comuna","region"), all=TRUE)
results <- merge(results, groupq, by=c("comuna","region"), all=TRUE)
results <- merge(results, healthinsure, by=c("comuna","region"), all=TRUE)
results <- merge(results, hospital, by=c("comuna","region"), all=TRUE)
results <- merge(results, icu, by=c("comuna","region"), all=TRUE)
results <- merge(results, incidence65, by=c("comuna","region"), all=TRUE)
results <- merge(results, incidencem, by=c("comuna","region"), all=TRUE)
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
  results <- results[-c(1,2)]
  names(results)[names(results)=="estado"] <- "region"
  names(results)[names(results)=="comuna_nome"] <- "comuna"
  results[is.na(results)] <- 0


#-------------------#
# EXPORT THE SCORES #
#-------------------#

write.csv(results, 'C:/Users/wkz8/Desktop/2020.03.14_PAHO_COVID-19/risk assessment tool/ciudad_mexico/results.csv', row.names = TRUE)



#-------------#
# END DO FILE #
#-------------#

