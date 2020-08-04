# Load packages needed for Santiago

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
options(shiny.maxRequestSize=2650*1024^2)


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
            fileInput("file1", "Choose CSV Files",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Horizontal line ----
            tags$hr(),
            
            helpText("Toggle Settings for uploading CSV"),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            helpText("Toggle Settings for viewing results"),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all",
                                     Tail = "tail"
                         ),
                         selected = "tail")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tabsetPanel( #type = "tabs",
                tabPanel("Welcome", 
                         tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
                         withMathJax(includeMarkdown("/Users/carinapeng/PAHO-LAC/welcome.md")),
                         h3(textOutput("upload")),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value"))),
                         # Display uploaded csv file
                         tableOutput("contents")
                ),
                tabPanel("Data",
                         hr(),
                         uiOutput("selectfile"),
                         selectInput('mydropdown', label = 'Select Neighborhood', choices = 'No choices here yet'),
                         uiOutput("selectfile2"),
                         uiOutput("selectfile3"),
                         textInput("phsm_country", label = "Type Country Name from PHSM Dataset", ""),
                         textInput("phsm_area", label = "Type Neighborhood Name from PHSM Dataset", "")),
                tabPanel("Individual Vunerability Index",
                         h3("Context"),
                         h4("Demographic and Socio-economic Information about the Neighborhood"),
                         fluidRow(
                             column(4,
                                    sliderInput("pop_dens", label = h4("1. Population density / Km²"), min = 0, 
                                                max = 25, value = 0, step = 0.1),
                                    verbatimTextOutput("pop_dens01")#,
                                    #verbatimTextOutput("testing")
                                    ),
                             column(4,
                                    sliderInput("water", label = h4("2. Availability of water and soap for hand washing inside the home"), min = 0, 
                                                max = 100, value = 0),
                                    verbatimTextOutput("water02")
                                    ),
                             column(4,
                                    sliderInput("occupation", label = h4("3. Proportion of the population who is staffing an essential worker position"), min = 0, 
                                                max = 70, value = 0),
                                    verbatimTextOutput("occupation03")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("workout", label = h4("4. Proportion of the population working outside the home"), min = 0, 
                                                max = 70, value = 0),
                                    verbatimTextOutput("workout04")
                                    ),
                             column(4,
                                    sliderInput("publictrans", label = h4("5. Proportion of the population who uses public transport"), min = 10, 
                                                max = 100, value = 0),
                                    verbatimTextOutput("publictrans05")
                                    ),
                             column(4,
                                    sliderInput("comorbidity", label = h4("6. Proportion of persons with pre-existing comorbidities"), min = 0, 
                                                max = 3, value = 0, step = 1),
                                    verbatimTextOutput("comorbidity06")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("vac_children", label = h4("7. Under- or non-vaccinated population: children younger than 1"), min = 0, 
                                                max = 100, value = 0, step = 1),
                                    verbatimTextOutput("vac_children07")
                                    ),
                             column(4,
                                    sliderInput("vac_elder", label = h4("8. Under- or non-vaccinated population: persons age 60 or older"), min = 0, 
                                                max = 100, value = 0, step = 1),
                                    verbatimTextOutput("vac_elder08")
                             ),
                             column(4,
                                    sliderInput("stunted", label = h4("9. Proportion of the population who is stunted"), min = 0, 
                                                max = 100, value = 0),
                                    verbatimTextOutput("stunted09")
                                    )
                         ),
                         h4("Individual Vunerability Score"),
                         verbatimTextOutput("context"),
                         tags$head(tags$style("#context{color: #0085b2;
                                 font-size: 16px;
                                 }"
                         )),
                         withMathJax(includeMarkdown("/Users/carinapeng/PAHO-LAC/context.md"))
                ),
                tabPanel("Social Vunerability Index",
                         h3("Context"),
                         h4("Demographic and Socio-economic Information about the Neighborhood"),
                         fluidRow(
                             column(4,
                                    sliderInput("employment", label = h4("10. Unemployment"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("employment10")
                                    
                             ),
                             column(4,
                                    sliderInput("income", label = h4("11. Per capita income"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("income11")
                             ),
                             column(4,
                                    sliderInput("education", label = h4("12. High school diploma"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("education12")
                             )),
                         fluidRow(
                             column(4,
                                    sliderInput("edad65", label = h4("13. Age 65 or older"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("edad65_13")
                             ),
                             column(4,
                                    sliderInput("edad17", label = h4("14. Age 17 or younger"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("edad17_14")
                             ),
                             column(4,
                                    sliderInput("disability", label = h4("15. Disability"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("disability15")
                             )),
                         fluidRow(
                             column(4,
                                    sliderInput("single_house", label = h4("16. Single parent household"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("single_house16")
                             ),
                             column(4,
                                    sliderInput("ethnic", label = h4("17. Ethinic minority"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("ethnic17")
                             ),
                             column(4,
                                    sliderInput("multihouse", label = h4("18. Multi-unit housing"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("multihouse18")
                             )
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("nbpersons", label = h4("19. Crowded household"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("nbpersons19")
                             ),
                             column(4,
                                    sliderInput("vehicle", label = h4("20. Vehicle availability"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("vehicle20")
                             ),
                             column(4,
                                    sliderInput("groupq", label = h4("21. Group quarters"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("groupq21")
                             )),
                         h4("Social Vunerability Score"),
                         tags$head(tags$style('h1 {color:red;}')),
                         verbatimTextOutput("context2"),
                         tags$head(tags$style("#context2{color: #0085b2;
                                 font-size: 16px;
                                 }"
                         )),
                         withMathJax(includeMarkdown("/Users/carinapeng/PAHO-LAC/context2.md"))
                ),
                tabPanel("Epidemiology", 
                         h3("Epidemiological Situation of the COVID-19 Pandemic in the Neighborhood"),
                         h4("Measure of Transmission"),
                         fluidRow(
                             column(4,
                                    sliderInput("inc", label = h4("1. Incidence rate per 100,000 population"), min = 0, 
                                                max = 1, value = 0, step = 1)
                             ),
                             column(4,
                                    sliderInput("inc_men", label = h4("2. Incidence rate among men"), min = 0, 
                                                max = 1, value = 0, step = 1)
                             ),
                             column(4,
                                    sliderInput("inc_elder", label = h4("3. Incidence rate among the elderly"), min = 0, 
                                                max = 1, value = 0, step = 1)
                             ),
                             column(4,
                                    sliderInput("perc_pos", label = h4("4. Percent positivity rate (%)"), min = 0, 
                                                max = 1, value = 0, step = 1)
                             )
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("doub", label = h4("5. Doubling rate for cases"), min = 0, 
                                                max = 3, value = 0, step = 1)
                             ),
                             column(4,
                                    sliderInput("type", label = h4("6. Type of COVID-19 transmission in the neighborhood"), min = 0, 
                                                max = 3, value = 0, step = 1)
                             ),
                             column(4,
                                    sliderInput("Rt", label = h4("7. Neighborhood-specific reproductive rate (Rt)"), min = 0, 
                                                max = 2, value = 0, step = 1),
                                    verbatimTextOutput("Rt07"),
                                    verbatimTextOutput("Rt07_disable")
                             ),
                             column(4,
                                    sliderInput("adj", label = h4("8. Presence of COVID-19 hotspot in an adjacent neighborhood"), min = 0, 
                                                max = 1, value = 0, step = 1)
                             )
                         ),
                         #verbatimTextOutput("contents5"),
                         #plotOutput("plot1"),
                         #plotOutput("plot2"),
                         h4("Measure of Mortality"),
                         fluidRow(
                             column(4,
                                    sliderInput("case_mort", label = h4("9. Case mortality rate "), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("case_mort9")
                             ),
                             column(4,
                                    sliderInput("case_men", label = h4("10. Case lethality rate among men"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("case_men10")
                             ),
                             column(4,
                                    sliderInput("case_elder", label = h4("11. Case lethality rate among the elderly"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("case_elder11")
                             ),
                             column(4,
                                    sliderInput("excess_death", label = h4("12. Number of excess deaths compared to 2015-2019"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("excess_death12")
                             )
                         )
                ),
                tabPanel("Mitigation",
                         h3("HEALTHCARE SYSTEMS TO TEST AND TREAT COVID-19 CASES - To be updated each month"),
                         #sliderTextInput(inputId = "social_dist", label = "16. Physical and social distancing and movement measures in place", grid = TRUE, force_edges = TRUE,
                             #choices = c("No", "Some", "Yes")
                         #),
                         fluidRow(
                             column(4, h4("16. Physical and social distancing and movement measures in place"),
                                    verbatimTextOutput("social_dist16")),
                             column(4, h4("17. Implementation of self-isolation and forced isolation measures"),
                                    verbatimTextOutput("isolation17")),
                             column(4, h4("18. Implementation of support measures during self-isolation/quarantine"),
                                    verbatimTextOutput("support18"))
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("speed", label = h4("19. Speed of isolation for new cases"), min = 0, 
                                                max = 100, value = 0, step = 1),
                                    verbatimTextOutput("speed19")
                             ),
                             column(4,
                                    sliderInput("case_qtine", label = h4("20. Proportion of cases identified among quarantined persons"), min = 0, 
                                                max = 100, value = 0, step = 1),
                                    verbatimTextOutput("case_qtine20")
                             ),
                             column(4, h4("21. Implementation of contact tracing"),
                                    verbatimTextOutput("contact_tracing21"))  
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("case_contact", label = h4("22. Proportion of cases identified from a contact list"), min = 0, 
                                                max = 100, value = 0, step = 1),
                                    verbatimTextOutput("case_contact22")
                             ),
                             column(4,
                                    sliderInput("prop_test", label = h4("23. Proportion of persons tested for every new COVID-19 case detected, in the last week"), min = 0, 
                                                max = 1, value = 0, step = 1),
                                    verbatimTextOutput("prop_test20")
                             ),
                             
                             column(4,
                                    h4("24. Mass gatherings"),
                                    verbatimTextOutput("gathering24"))
                         ),
                         fluidRow(
                             column(4,
                                 h4("25. Social disturbances"),
                                 verbatimTextOutput("disturbance25")
                             ),
                             column(4,
                                 sliderInput("prop_adhere", label = h4("26. Proportion of the population who adheres to mitigation measures"), min = 0, 
                                             max = 100, value = 0, step = 1),
                                 verbatimTextOutput("prop_adhere26")
                             )
                ), 
                         #tableOutput("phsm_table_test"),
                         h4("Public Health and Social Measures Score"),
                tags$head(tags$style('h1 {color:red;}')),
                verbatimTextOutput("miti3"),
                tags$head(tags$style("#miti3{color: #0085b2;
                                 font-size: 16px;
                                 }"
                ))
            ),
            tabPanel("Score",
                     h3("Scores & Interpretation"),
                     fluidRow(
                        column(5,
                               h4("Composite Scores"),
                               tableOutput("tbl")
                        ),
                        column(5,
                               h4("Vunerability Score"),
                               tags$head(tags$style('h1 {color:red;}')),
                               verbatimTextOutput("final_vun_output"),
                               tags$head(tags$style("#final_vun_output{color: #0085b2;
                                 font-size: 16px;
                                 }"
                               )),
                               verbatimTextOutput("final_vun_text"),
                               tags$head(tags$style("#final_vun_text{color: #0085b2;
                                 font-size: 16px;
                                 }"
                               ))
                               ),
                        column(5,
                               h4("Mitigation Score"),
                               tags$head(tags$style('h1 {color:red;}')),
                               verbatimTextOutput("final_miti"),
                               tags$head(tags$style("#final_miti{color: #0085b2;
                                 font-size: 16px;
                                 }"
                               )),
                               verbatimTextOutput("final_miti_text"),
                               tags$head(tags$style("#final_miti_text{color: #0085b2;
                                 font-size: 16px;
                                 }"
                               ))
                               ),
                     ),
                     #tags$img(src = "matrix1.png", height = 200, width = 400),
                     tags$img(src = "matrix2.png", height = 400, width = 450))
        ))))
        

# SERVER    
server <- function(input, output, session) { 

    
    output$selectfile <- renderUI({
        if(is.null(input$file1)) {return()}
        list(hr(), 
             selectInput("Select_test", "Select Census Data", choices=input$file1$name)
        )
    })
    
    output$selectfile2 <- renderUI({
        if(is.null(input$file1)) {return()}
        list(hr(),
             selectInput("Select_test2", "Select Incidence Data", choices=input$file1$name)
        )
    })
    
    output$selectfile3 <- renderUI({
        if(is.null(input$file1)) {return()}
        list(hr(),
             selectInput("Select_test3", "Select WHO PHSM Data", choices=input$file1$name)
        )
    })
    
    
    file01 <- reactive({
        req(input$file1)
        read.csv(file=input$file1$datapath[input$file1$name==input$Select_test],
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    file02 <- reactive({
        req(input$file1)
        read.csv(file=input$file1$datapath[input$file1$name==input$Select_test2],
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    file03 <- reactive({
        req(input$file1)
        read.csv(file=input$file1$datapath[input$file1$name==input$Select_test3],
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    })
    
    
    observeEvent(input$Select_test, {
        updateSelectInput(session, "mydropdown", label = "Select neighborhood", choices = file01()[1])
    })
    
    municipal <- reactive({
        req(input$mydropdown)
        file01_municipal <- file01() %>%
            filter(comuna == input$mydropdown)
        return(file01_municipal)
    })
    
    phsm_municipal <- reactive({
        req(input$phsm_country)
        req(input$phsm_area)
        file03_municipal1 <- file03() %>%
            filter(country_territory_area == input$phsm_country) %>%
            filter(admin_level == "national")
        file03_municipal2 <- file03() %>%
             filter(area_covered == input$phsm_area)
        file03_municipal <- rbind(file03_municipal1, file03_municipal2)
        
        social = ifelse("Social and physical distancing measures" %in% x$who_category, 0, 1)
        isolation = ifelse("Surveillance and response measures" %in% x$who_category, 0, 1)
        support = ifelse("Social and physical distancing measures" %in% x$who_category, 0, 1)
        contact = ifelse("Tracing and quarantining contacts" %in% x$who_subcategory, 0, 1)
        gathering = ifelse("Gatherings, businesses and services" %in% x$who_subcategory, 0, 1)
        disturbance = ifelse("Gatherings, businesses and services" %in% x$who_subcategory, 0, 1)
        
        combined <- data.frame(value = rbind(social, isolation, support, contact, gathering, disturbance))
        combined_transpose <- data.frame(transpose(combined))
        rownames(combined_transpose) <- colnames(combined)
        colnames(combined_transpose) <- rownames(combined)
        
        return(combined_transpose)
    })

    
    # TEST - Show the table for filtered PHSM excel with selected municipal
    #output$phsm_table_test <- renderTable({
        #req(input$phsm_country)
        #req(input$phsm_area)
        #return(phsm_municipal())
    #})
    
    df <- reactive({
        req(input$file1)
        x = file02()
        x[,1]<-as.Date(x[,1], "%d/%m/%Y")
        dfR <- estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))
        return(dfR)
    })
    
    
    #output$plot1 <- renderPlot({
        #plot(df(), what=c("incid"))
    #})
    #output$plot2 <- renderPlot({
        #plot(df(), what=c("R"))
    #})
    
    pop_dens_coded <- reactive({
        req(input$pop_dens)
        if (input$pop_dens >= 0.3&input$pop_dens <= 0.9) {
            return(0)
        } else if (input$pop_dens >= 0.91&input$pop_dens <= 2.45) {
            return(1)
        } else if (input$pop_dens >= 2.46&input$pop_dens <= 8.08) {
            return(2)
        } else if (input$pop_dens >= 8.09&input$pop_dens <= 22.5) {
            return(3)
        } else if (input$pop_dens >= 22.51) {
            return(4)
        }
    })
    
    #output$testing <- renderText(contexto01())
    
    contexto01 <- reactive({
        if (is.null(municipal()$contexto01)) {
            return(pop_dens_coded())
        }
        else {
            return(as.character(municipal()$contexto01))
        }
    })
    
    water_coded <- reactive({
        req(input$water)
        if (input$water>75) {
            return(0)
        } else if (input$water>=50&input$water<=75) {
            return(1)
        } else if (input$water<50) {
            return(2)
        }
    })
    
    contexto02 <- reactive({
        if (is.null(municipal()$contexto02)) {
            return(water_coded())
        }
        else {
            return(as.character(municipal()$contexto02))
        }
    })
    
    occupation_coded <- reactive({
        req(input$occupation)
        if (input$occupation<=10) {
            return(0)
        } else if (input$occupation>=11&input$occupation<=30) {
            return(1)
        } else if (input$occupation>=31) {
            return(2)
        }
    })
    
    contexto03 <- reactive({
        if (is.null(municipal()$contexto03)) {
            return(occupation_coded())
        }
        else {
            return(as.character(municipal()$contexto03))
        }
    })
    
    workout_coded <- reactive({
        req(input$workout)
        if (input$workout<=10) {
            return(0)
        } else if (input$workout>=11&input$workout<=30) {
            return(1)
        } else if (input$workout>=31) {
            return(2)
        }
    })
    
    contexto04 <- reactive({
        if (is.null(municipal()$contexto04)) {
            return(input$workout)
        }
        else {
            return(as.character(municipal()$contexto04))
        }
    })
    
    publictrans_coded <- reactive({
        req(input$publictrans)
        if (input$publictrans<50) {
            return(0)
        } else if (input$publictrans>=51&input$publictrans<=65) {
            return(1)
        } else if (input$publictrans>=65) {
            return(2)
        }
    })
    
    contexto05 <- reactive({
        if (is.null(municipal()$contexto05)) {
            return(publictrans_coded())
        }
        else {
            return(as.character(municipal()$contexto05))
        }
    })
    
    contexto06 <- reactive({
        if (is.null(municipal()$contexto06)) {
            return(input$comorbidity)
        }
        else {
            return(as.character(municipal()$contexto06))
        }
    })
    
    contexto07 <- reactive({
        if (is.null(municipal()$contexto07)) {
            return(input$vac_children)
        }
        else {
            return(as.character(municipal()$contexto07))
        }
    })
    
    contexto08 <- reactive({
        if (is.null(municipal()$contexto08)) {
            return(input$vac_elder)
        }
        else {
            return(as.character(municipal()$contexto08))
        }
    })
    
    stunted_coded <- reactive({
        req(input$stunted)
        if (input$stunted<30) {
            return(0)
        } else if (input$stunted>=31) {
            return(1)
        }
    })
    
    contexto09 <- reactive({
        if (is.null(municipal()$contexto09)) {
            return(stunted_coded())
        }
        else {
            return(as.character(municipal()$contexto09))
        }
    })
    
    contexto10 <- reactive({
        if (is.null(municipal()$contexto10)) {
            return(input$employment)
        }
        else {
            return(as.character(municipal()$contexto10))
        }
    })
    
    contexto11 <- reactive({
        if (is.null(municipal()$contexto11)) {
            return(input$income)
        }
        else {
            return(as.character(municipal()$contexto11))
        }
    })
    
    contexto12 <- reactive({
        if (is.null(municipal()$contexto12)) {
            return(input$education)
        }
        else {
            return(as.character(municipal()$contexto12))
        }
    })
    
    contexto13 <- reactive({
        if (is.null(municipal()$contexto13)) {
            return(input$edad65)
        }
        else {
            return(as.character(municipal()$contexto13))
        }
    })
    
    contexto14 <- reactive({
        if (is.null(municipal()$contexto14)) {
            return(input$edad17)
        }
        else {
            return(as.character(municipal()$contexto14))
        }
    })
    
    contexto15 <- reactive({
        if (is.null(municipal()$contexto15)) {
            return(input$disability)
        }
        else {
            return(as.character(municipal()$contexto15))
        }
    })
    
    contexto16 <- reactive({
        if (is.null(municipal()$contexto16)) {
            return(input$single_house)
        }
        else {
            return(as.character(municipal()$contexto16))
        }
    })
    
    contexto17 <- reactive({
        if (is.null(municipal()$contexto17)) {
            return(input$ethnic)
        }
        else {
            return(as.character(municipal()$contexto17))
        }
    })
    
    contexto18 <- reactive({
        if (is.null(municipal()$contexto18)) {
            return(input$multihouse)
        }
        else {
            return(as.character(municipal()$contexto18))
        }
    })
    
    contexto19 <- reactive({
        if (is.null(municipal()$contexto19)) {
            return(input$nbpersons)
        }
        else {
            return(as.character(municipal()$contexto19))
        }
    })
    
    contexto20 <- reactive({
        if (is.null(municipal()$contexto20)) {
            return(input$vehicle)
        }
        else {
            return(as.character(municipal()$contexto20))
        }
    })
    
    contexto21 <- reactive({
        if (is.null(municipal()$contexto21)) {
            return(input$groupq)
        }
        else {
            return(as.character(municipal()$contexto21))
        }
    })
    
    
    output$pop_dens01 <- renderPrint({
        if (is.null(municipal()$contexto01)) {
            #return(writeLines(c("Value NOT found", contexto01())))
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(list(writeLines(c(contexto01())), disable("pop_dens")))
            return(disable("pop_dens"))
        }
    })
    
    output$water02 <- renderPrint({
        if (is.null(municipal()$contexto02)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("water")) 
        }
    })
    
    output$occupation03 <- renderPrint({
        if (is.null(municipal()$contexto03)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto03())))
            return(disable("occupation"))
        }
    })
    
    output$workout04 <- renderPrint({
        if (is.null(municipal()$contexto04)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto04())))
            return(disable("workout"))
        }
    })
    
    output$publictrans05 <- renderPrint({
        if (is.null(municipal()$contexto05)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto05())))
            return(disable("publictrans"))
        }
    })
    
    output$comorbidity06 <- renderPrint({
        if (is.null(municipal()$contexto06)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto06())))
            return(disable("comorbidity"))
        }
    })
    
    output$vac_children07 <- renderPrint({
        if (is.null(municipal()$contexto07)) {
            return(writeLines("Please enter value: Proportion of children younger than 1 that has vaccination coverage with three doses of DPT vaccine."))
        }
        
        else {
            #return(writeLines(c(contexto07())))
            return(disable("vac_children"))
        }
    })
    
    output$vac_elder08 <- renderPrint({
        if (is.null(municipal()$contexto08)) {
            return(writeLines("Please enter value: Proportion of persons age 60 or older that has vaccination coverage with one dose of influenza vaccine in the previous year."))
        }
        
        else {
            #return(writeLines(c(contexto08())))
            return(disable("vac_elder"))
        }
    })
    
    output$stunted09 <- renderPrint({
        if (is.null(municipal()$contexto09)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto09())))
            return(disable("stunted"))
        }
    })
    
    output$employment10 <- renderPrint({
        if (is.null(municipal()$contexto10)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("employment"))
        }
    })
    
    output$income11 <- renderPrint({
        if (is.null(municipal()$contexto11)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("income"))
        }
    })
    
    output$education12 <- renderPrint({
        if (is.null(municipal()$contexto12)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("education"))
        }
    })
    
    output$edad65_13 <- renderPrint({
        if (is.null(municipal()$contexto13)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("edad65"))
        }
    })
    
    output$edad17_14 <- renderPrint({
        if (is.null(municipal()$contexto14)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("edad17"))
        }
    })
    
    output$disability15 <- renderPrint({
        if (is.null(municipal()$contexto15)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("disability"))
        }
    })
    
    output$single_house15 <- renderPrint({
        if (is.null(municipal()$contexto16)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("single_house"))
        }
    })
    
    output$ethnic17 <- renderPrint({
        if (is.null(municipal()$contexto17)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("ethnic"))
        }
    })
    
    output$multihouse18 <- renderPrint({
        if (is.null(municipal()$contexto18)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("multihouse"))
        }
    })
    
    output$nbpersons19 <- renderPrint({
        if (is.null(municipal()$contexto19)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("nbpersons"))
        }
    })
    
    output$vehicle20 <- renderPrint({
        if (is.null(municipal()$contexto20)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("vehicle"))
        }
    })
    
    output$groupq21 <- renderPrint({
        if (is.null(municipal()$contexto21)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("groupq"))
        }
    })
    
    output$social_dist16 <- renderPrint({
        if (phsm_municipal()$social == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$isolation17 <- renderPrint({
        if (phsm_municipal()$isolation == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$support18 <- renderPrint({
        if (phsm_municipal()$support == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    speed_coded <- reactive({
        req(input$speed)
        if (input$speed >= 80) {
            return(0)
        } else if (input$speed >= 50&input$speed <= 79) {
            return(1)
        } else if (input$speed <= 50) {
            return(2)
        }
    })
    
    case_qtine_coded <- reactive({
        req(input$case_qtine)
        if (input$case_qtine >= 80) {
            return(0)
        } else if (input$case_qtine >= 50&input$case_qtine <= 79) {
            return(1)
        } else if (input$case_qtine <= 50) {
            return(2)
        }
    })
    
    output$contact_tracing21 <- renderPrint({
        if (phsm_municipal()$contact == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    case_contact_coded <- reactive({
        req(input$case_contact)
        if (input$case_contact >= 80) {
            return(0)
        } else if (input$case_contact >= 50&input$case_contact <= 79) {
            return(1)
        } else if (input$case_contact <= 50) {
            return(2)
        }
    })
    
    output$gathering24 <- renderPrint({
        if (phsm_municipal()$gathering == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$disturbance25 <- renderPrint({
        if (phsm_municipal()$disturbance == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    prop_adhere_coded <- reactive({
        req(input$prop_adhere)
        if (input$prop_adhere > 75) {
            return(0)
        } else if (input$prop_adhere >= 51&input$prop_adhere <= 75) {
            return(1)
        } else if (input$prop_adhere < 50) {
            return(2)
        }
    })

    individual <- reactive({
        individual_vunerability <- (as.numeric(contexto01())*2 + 
                                        as.numeric(contexto02())*1 + 
                                        as.numeric(contexto03())*1 + 
                                        as.numeric(contexto04())*1 +
                                        as.numeric(contexto05())*1 +
                                        as.numeric(contexto06())*3 +
                                        as.numeric(contexto07())*2 +
                                        as.numeric(contexto08())*2 +
                                        as.numeric(contexto09())*2
                                    )
        return(as.numeric(individual_vunerability))
    })
    
    
    output$context <- renderPrint({
        return(writeLines(as.character(individual())))
    })
    
    individual <- reactive({
        individual_vunerability <- (as.numeric(contexto01())*2 + 
                                        as.numeric(contexto02())*1 + 
                                        as.numeric(contexto03())*1 + 
                                        as.numeric(contexto04())*1 +
                                        as.numeric(contexto05())*1 +
                                        as.numeric(contexto06())*3 +
                                        as.numeric(contexto07())*2 +
                                        as.numeric(contexto08())*2 +
                                        as.numeric(contexto09())*2
        )
        return(individual_vunerability)
    })
    
    social <- reactive({
        social_vunerability <- (as.numeric(contexto10())*1 +
                                    as.numeric(contexto11())*1 +
                                    as.numeric(contexto12())*1 +
                                    as.numeric(contexto13())*2 +
                                    as.numeric(contexto14())*1 +
                                    as.numeric(contexto15())*1 +
                                    as.numeric(contexto16())*1 +
                                    as.numeric(contexto17())*2 +
                                    as.numeric(contexto18())*2 +
                                    as.numeric(contexto19())*3 +
                                    as.numeric(contexto20())*1 +
                                    as.numeric(contexto21())*3
        )
        return(social_vunerability)
    })
    
    output$context2 <- renderPrint({
        return(writeLines(as.character(social())))
    })
    
    public_health <- reactive({
        public_health_measures <- (as.numeric(phsm_municipal()$social)*3 +
                                       as.numeric(phsm_municipal()$isolation)*3 +
                                       as.numeric(phsm_municipal()$support)*3 +
                                       as.numeric(speed_coded())*3 +
                                       as.numeric(case_qtine_coded())*3 +
                                       as.numeric(phsm_municipal()$contact)*2 +
                                       as.numeric(case_contact_coded())*3 +
                                       as.numeric(input$prop_test)*1 +
                                       as.numeric(phsm_municipal()$gathering)*2 +
                                       as.numeric(phsm_municipal()$disturbance)*2 +
                                       as.numeric(prop_adhere_coded())*3
        )
        return(public_health_measures)
    })
    
    output$miti3 <- renderPrint({
        return(writeLines(as.character(public_health())))
    })
    
    Rt_raw <- reactive({
        x <- df()$R$Mean
        return(round(x[length(x)],digits=2))
    })
    
    
    #multi_return <- function(){
        #my_list <- list(c(disable("Rt"), paste("The current reproductive number (R) is estimated to be", Rt_raw())))
        #return(my_list)
    #}
    
    output$Rt07 <- renderPrint({
        if (is.null(Rt_raw())) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(((multi_return())))
            #return(disable("Rt"))
            #return(cat(paste("The current reproductive number (R) is estimated to be", Rt_raw())))
            return(cat(Rt_raw()))
        }
    })
    
    output$Rt07_disable <- renderPrint({
        if (is.null(Rt_raw())) {
            return()
        }
        
        else {
            return(disable("Rt"))
        }
    })
    
    #output$contents5 <- renderPrint({
        #return(writeLines(c("The current reproductive number (R) is estimated to be", Rt())))
    #})
        
    epi_transmission <- reactive({
        epi_trans <- (as.numeric(Rt())*2)
        return(epi_trans)
    })
    
    epi_mortality <- reactive({
        epi_mortality_input <- (as.numeric(input$case_mort)*3 +
                                    as.numeric(input$case_men)*1 +
                                    as.numeric(input$case_elder)*1 +
                                    as.numeric(input$excess_death)*2
        )
        return(epi_mortality_input)
    })
    
    output$tbl <- renderTable({
        final_df <- data.frame(
            Category = c(
                "Context - Individual vulnerability",
                "Context - Social vulnerability",
                "Epidemiology - Transmission",
                "Epidemiology - Social vulnerability",
                "Healthcare Systems - Transmission",
                "Healthcare Systems - Mortality"
            ),
            Score = c(individual(),
                      social(),
                      epi_transmission(),
                      epi_mortality(),
                      social(),
                      individual())
        )
        
        return(final_df)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # FIX CODE AFTER PRESENTATION
    final_vun <- reactive({
        final_vunerability <- (as.numeric(individual()) +
                                       as.numeric(social()) +
                                       as.numeric(epi_transmission()) +
                                       as.numeric(epi_mortality()) +
                                       as.numeric(social()) +
                                       as.numeric(individual())
        )
        return(final_vunerability)
    })
    
    output$final_vun_output <- renderPrint({
        return(writeLines(as.character(final_vun())))
    })
    
    final_vun_text <- reactive({
        req(final_vun())
        if (final_vun() >= 0 & final_vun() <= 30) {
            return("Minimal")
        } else if (final_vun() >= 31 & final_vun() <= 60) {
            return("Low")
        } else if (final_vun() >= 61 & final_vun() <= 90) {
            return("Moderate Risk")
        } else if (final_vun() >= 91 & final_vun() <= 120) {
            return("High")
        } else if (final_vun() >= 121 & final_vun() <= 144) {
            return("Very High Risk")
        }
    })
    
    output$final_vun_text <- renderPrint({
        return(writeLines(as.character(final_vun_text())))
    })
    
    final_miti_text <- reactive({
        req(public_health())
        if (public_health() >= 0 & public_health() <= 10) {
            return("Very High")
        } else if (public_health() >= 11 & public_health() <= 20) {
            return("High")
        } else if (public_health() >= 21 & public_health() <= 30) {
            return("Moderate")
        } else if (public_health() >= 31 & public_health() <= 40) {
            return("Low")
        } else if (public_health() >= 41 & public_health() <= 51) {
            return("Minimal")
        }
    })
    
    output$final_miti_text <- renderPrint({
        return(writeLines(as.character(final_miti_text())))
    })
    
    output$final_miti <- renderPrint({
        return(writeLines(as.character(public_health())))
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)

