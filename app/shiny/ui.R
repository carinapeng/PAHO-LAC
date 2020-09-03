library(lubridate)
library(data.table)
library(ggplot2)
library(ggthemes)
library(reshape)
library(reshape2)
library(tidyr)
library(tidyverse)
library(scales)
library(magrittr)
library(dplyr)
library(pyramid)
#library(XML)
library(plyr)
library(foreign)
library(ggpol)

# Load packages needed for Shiny App
library(EpiEstim)
library(markdown)
library(shiny)
library(shinyjs)
library(knitr)
library(ggplot2)
library(incidence)
library(data.table)
library(shinyWidgets)
library(DT)
options(shiny.maxRequestSize = 2650 * 1024 ^ 2)

setwd("/Users/carinapeng/PAHO-LAC")

# Define UI for data upload app ----
ui <- fluidPage(
    useShinyjs(),
    
    # App title ----
    titlePanel("PAHO Risk Assessment Tool"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Select a file ----
            fileInput(
                "file1",
                "Choose CSV Files",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            # Horizontal line ----
            tags$hr(),
            
            helpText("Toggle Settings for uploading CSV"),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons(
                "sep",
                "Separator",
                choices = c(
                    Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"
                ),
                selected = ","
            ),
            
            # Input: Select quotes ----
            radioButtons(
                "quote",
                "Quote",
                choices = c(
                    None = "",
                    "Double Quote" = '"',
                    "Single Quote" = "'"
                ),
                selected = '"'
            ),
            
            # Horizontal line ----
            tags$hr(),
            helpText("Toggle Settings for viewing results"),
            
            # Input: Select number of rows to display ----
            radioButtons(
                "disp",
                "Display",
                choices = c(Head = "head",
                            All = "all",
                            Tail = "tail"),
                selected = "tail"
            )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Data file ----
            tabsetPanel(
                #type = "tabs",
                tabPanel(
                    "Welcome",
                    tags$div(
                        HTML(
                            "<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            "
                        )
                    ),
                    withMathJax(includeMarkdown(
                        "app/www/welcome.md"
                    )),
                    h3(textOutput("upload")),
                    hr(),
                    fluidRow(column(3, verbatimTextOutput("value"))),
                    # Display uploaded csv file
                    tableOutput("contents")
                ),
                tabPanel(
                    "Data",
                    hr(),
                    #uiOutput("selectfile"),
                    selectInput("dropdown_country", label = "Select Country", choices = c("Mexico", "Chile")),
                    uiOutput("dropdown_city"),
                    uiOutput("selectfile_incidence"),
                    uiOutput("selectfile_policy"),
                    textInput("phsm_country", label = "Type Country Name from PHSM Dataset", ""),
                    textInput("phsm_area", label = "Type Neighborhood Name from PHSM Dataset", "")
                ),
                tabPanel(
                    "Individual Vunerability Index",
                    h3("Context"),
                    h4(
                        "Demographic and Socio-economic Information about the Neighborhood"
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "pop_dens",
                                label = h4("1. Population density / Km²"),
                                min = 0,
                                max = 25,
                                value = 0,
                                step = 0.1
                            ),
                            verbatimTextOutput("pop_dens01")
                        ),
                        column(
                            4,
                            sliderInput(
                                "water",
                                label = h4("2. Availability of water and soap for hand washing inside the home"),
                                min = 0,
                                max = 100,
                                value = 0
                            ),
                            verbatimTextOutput("water02")
                        ),
                        column(
                            4,
                            sliderInput(
                                "occupation",
                                label = h4(
                                    "3. Proportion of the population who is staffing an essential worker position"
                                ),
                                min = 0,
                                max = 70,
                                value = 0
                            ),
                            verbatimTextOutput("occupation03")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "workout",
                                label = h4("4. Proportion of the population working outside the home"),
                                min = 0,
                                max = 70,
                                value = 0
                            ),
                            verbatimTextOutput("workout04")
                        ),
                        column(
                            4,
                            sliderInput(
                                "publictrans",
                                label = h4("5. Proportion of the population who uses public transport"),
                                min = 10,
                                max = 100,
                                value = 0
                            ),
                            verbatimTextOutput("publictrans05")
                        ),
                        column(
                            4,
                            sliderInput(
                                "comorbidity",
                                label = h4("6. Proportion of persons with pre-existing comorbidities"),
                                min = 0,
                                max = 3,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("comorbidity06")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "vac_children",
                                label = h4("7. Under- or non-vaccinated population: children younger than 1"),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("vac_children07")
                        ),
                        column(
                            4,
                            sliderInput(
                                "vac_elder",
                                label = h4("8. Under- or non-vaccinated population: persons age 60 or older"),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("vac_elder08")
                        ),
                        column(
                            4,
                            sliderInput(
                                "stunted",
                                label = h4("9. Proportion of the population who is stunted"),
                                min = 0,
                                max = 100,
                                value = 0
                            ),
                            verbatimTextOutput("stunted09")
                        )
                    ),
                    h4("Individual Vunerability Score"),
                    verbatimTextOutput("context"),
                    tags$head(tags$style(
                        "#context{color: #0085b2;
                                 font-size: 16px;
                                 }"
                    )),
                    withMathJax(includeMarkdown(
                        "app/www/context.md"
                    ))
                ),
                tabPanel(
                    "Social Vunerability Index",
                    h3("Context"),
                    h4(
                        "Demographic and Socio-economic Information about the Neighborhood"
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "employment",
                                label = h4("10. Unemployment"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("employment10")
                            
                        ),
                        column(
                            4,
                            sliderInput(
                                "income",
                                label = h4("11. Per capita income"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("income11")
                        ),
                        column(
                            4,
                            sliderInput(
                                "education",
                                label = h4("12. High school diploma"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("education12")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "edad65",
                                label = h4("13. Age 65 or older"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("edad65_13")
                        ),
                        column(
                            4,
                            sliderInput(
                                "edad17",
                                label = h4("14. Age 17 or younger"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("edad17_14")
                        ),
                        column(
                            4,
                            sliderInput(
                                "disability",
                                label = h4("15. Disability"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("disability15")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "single_house",
                                label = h4("16. Single parent household"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("single_house16")
                        ),
                        column(
                            4,
                            sliderInput(
                                "ethnic",
                                label = h4("17. Ethinic minority"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("ethnic17")
                        ),
                        column(
                            4,
                            sliderInput(
                                "multihouse",
                                label = h4("18. Multi-unit housing"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("multihouse18")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "nbpersons",
                                label = h4("19. Crowded household"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("nbpersons19")
                        ),
                        column(
                            4,
                            sliderInput(
                                "vehicle",
                                label = h4("20. Vehicle availability"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("vehicle20")
                        ),
                        column(
                            4,
                            sliderInput(
                                "groupq",
                                label = h4("21. Group quarters"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("groupq21")
                        )
                    ),
                    h4("Social Vunerability Score"),
                    tags$head(tags$style('h1 {color:red;}')),
                    verbatimTextOutput("context2"),
                    tags$head(tags$style(
                        "#context2{color: #0085b2;
                                 font-size: 16px;
                                 }"
                    )),
                    withMathJax(includeMarkdown(
                        "app/www/context2.md"
                    ))
                ),
                tabPanel(
                    "Epidemiology",
                    h3(
                        "Epidemiological Situation of the COVID-19 Pandemic in the Neighborhood"
                    ),
                    h4("Measure of Transmission"),
                    fluidRow(
                        column(4,
                               sliderInput(
                                   "inc",
                                   label = h4("1. Incidence rate per 100,000 population"),
                                   min = 0,
                                   max = 1,
                                   value = 0,
                                   step = 1
                               )),
                        column(
                            4,
                            sliderInput(
                                "inc_men",
                                label = h4("2. Incidence rate among men"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            )
                        ),
                        column(
                            4,
                            sliderInput(
                                "inc_elder",
                                label = h4("3. Incidence rate among the elderly"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "perc_pos",
                                label = h4("4. Percent positivity rate (%)"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            )
                        ),
                        column(4,
                               sliderInput(
                                   "doub",
                                   label = h4("5. Doubling rate for cases"),
                                   min = 0,
                                   max = 3,
                                   value = 0,
                                   step = 1
                               )),
                        column(4,
                               sliderInput(
                                   "type",
                                   label = h4("6. Type of COVID-19 transmission in the neighborhood"),
                                   min = 0,
                                   max = 3,
                                   value = 0,
                                   step = 1
                               ))
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "Rt",
                                label = h4("7. Neighborhood-specific reproductive rate (Rt)"),
                                min = 0,
                                max = 2,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("Rt07"),
                            verbatimTextOutput("Rt07_disable")
                        ),
                        column(4,
                               sliderInput(
                                   "adj",
                                   label = h4("8. Presence of COVID-19 hotspot in an adjacent neighborhood"),
                                   min = 0,
                                   max = 1,
                                   value = 0,
                                   step = 1
                               ))
                    ),
                    #verbatimTextOutput("contents5"),
                    #plotOutput("plot1"),
                    #plotOutput("plot2"),
                    h4("Measure of Mortality"),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "case_mort",
                                label = h4("9. Case mortality rate "),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("case_mort9")
                        ),
                        column(
                            4,
                            sliderInput(
                                "case_men",
                                label = h4("10. Case lethality rate among men"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("case_men10")
                        ),
                        column(
                            4,
                            sliderInput(
                                "case_elder",
                                label = h4("11. Case lethality rate among the elderly"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("case_elder11")
                        ),
                        column(
                            4,
                            sliderInput(
                                "excess_death",
                                label = h4("12. Number of excess deaths compared to 2015-2019"),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("excess_death12")
                        )
                    )
                ),
                tabPanel(
                    "Mitigation",
                    h3("Healthcare Systems tp Test and Treat COVID-19 Cases"),
                    #sliderTextInput(inputId = "social_dist", label = "16. Physical and social distancing and movement measures in place", grid = TRUE, force_edges = TRUE,
                    #choices = c("No", "Some", "Yes")
                    #),
                    fluidRow(
                        column(
                            4,
                            h4("16. Physical and social distancing and movement measures in place"),
                            verbatimTextOutput("social_dist16")
                        ),
                        column(
                            4,
                            h4(
                                "17. Implementation of self-isolation and forced isolation measures"
                            ),
                            verbatimTextOutput("isolation17")
                        ),
                        column(
                            4,
                            h4(
                                "18. Implementation of support measures during self-isolation/quarantine"
                            ),
                            verbatimTextOutput("support18")
                        )
                    ), 
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "speed",
                                label = h4("19. Speed of isolation for new cases"),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("speed19")
                        ),
                        column(
                            4,
                            sliderInput(
                                "case_qtine",
                                label = h4("20. Proportion of cases identified among quarantined persons"),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("case_qtine20")
                        ),
                        column(
                            4,
                            h4("21. Implementation of contact tracing"),
                            verbatimTextOutput("contact_tracing21")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            sliderInput(
                                "case_contact",
                                label = h4("22. Proportion of cases identified from a contact list"),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("case_contact22")
                        ),
                        column(
                            4,
                            sliderInput(
                                "prop_test",
                                label = h4(
                                    "23. Proportion of persons tested for every new COVID-19 case detected, in the last week"
                                ),
                                min = 0,
                                max = 1,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("prop_test20")
                        ),
                        
                        column(
                            4,
                            h4("24. Mass gatherings"),
                            verbatimTextOutput("gathering24")
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            h4("25. Social disturbances"),
                            verbatimTextOutput("disturbance25")
                        ),
                        column(
                            4,
                            sliderInput(
                                "prop_adhere",
                                label = h4(
                                    "26. Proportion of the population who adheres to mitigation measures"
                                ),
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 1
                            ),
                            verbatimTextOutput("prop_adhere26")
                        )
                    ),
                    tableOutput("df_test"),
                    h4("Public Health and Social Measures Score"),
                    tags$head(tags$style('h1 {color:red;}')),
                    verbatimTextOutput("miti3"),
                    tags$head(tags$style(
                        "#miti3{color: #0085b2;
                                 font-size: 16px;
                                 }"
                    ))
                ),
                tabPanel(
                    "Score",
                    h3("Scores & Interpretation"),
                    fluidRow(
                        column(5,
                               h4("Composite Scores"),
                               tableOutput("tbl")),
                        column(
                            5,
                            h4("Vunerability Score"),
                            tags$head(tags$style('h1 {color:red;}')),
                            verbatimTextOutput("final_vun_output"),
                            tags$head(
                                tags$style("#final_vun_output{color: #0085b2;
                                 font-size: 16px;
                                 }")
                            ),
                            verbatimTextOutput("final_vun_text"),
                            tags$head(
                                tags$style("#final_vun_text{color: #0085b2;
                                 font-size: 16px;
                                 }")
                            )
                        ),
                        column(
                            5,
                            h4("Mitigation Score"),
                            tags$head(tags$style('h1 {color:red;}')),
                            verbatimTextOutput("final_miti"),
                            tags$head(tags$style(
                                "#final_miti{color: #0085b2;
                                 font-size: 16px;
                                 }"
                            )),
                            verbatimTextOutput("final_miti_text"),
                            tags$head(
                                tags$style("#final_miti_text{color: #0085b2;
                                 font-size: 16px;
                                 }")
                            )
                        ),
                    ),
                    #tags$img(src = "matrix1.png", height = 200, width = 400),
                    tags$img(
                        src = "matrix2.png",
                        height = 400,
                        width = 450
                    )
                )
            )
        )
    )
)
