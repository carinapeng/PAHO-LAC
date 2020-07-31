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
                                                max = 25, value = 0),
                                    verbatimTextOutput("pop_dens01")#,
                                    #verbatimTextOutput("testing")
                                    ),
                             column(4,
                                    sliderInput("water", label = h4("2. Availability of water and soap for hand washing inside the home"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("water02")
                                    ),
                             column(4,
                                    sliderInput("occupation", label = h4("3. Proportion of the population who is staffing an essential worker position"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("occupation03")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("workout", label = h4("4. Proportion of the population working outside the home"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("workout04")
                                    ),
                             column(4,
                                    sliderInput("publictrans", label = h4("5. Proportion of the population who uses public transport"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("publictrans05")
                                    ),
                             column(4,
                                    sliderInput("comorbidity", label = h4("6. Proportion of persons with pre-existing comorbidities"), min = 0, 
                                                max = 3, value = 0),
                                    verbatimTextOutput("comorbidity06")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("vac_children", label = h4("7. Under- or non-vaccinated population: children younger than 1"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("vac_children07")
                                    ),
                             column(4,
                                    sliderInput("vac_elder", label = h4("8. Under- or non-vaccinated population: persons age 60 or older"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("vac_elder08")
                             ),
                             column(4,
                                    sliderInput("stunted", label = h4("9. Proportion of the population who is stunted"), min = 0, 
                                                max = 1, value = 0),
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
                                                max = 4, value = 0),
                                    verbatimTextOutput("employment10")
                                    
                             ),
                             column(4,
                                    sliderInput("income", label = h4("11. Per capita income"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("income11")
                             ),
                             column(4,
                                    sliderInput("education", label = h4("12. High school diploma"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("education12")
                             )),
                         fluidRow(
                             column(4,
                                    sliderInput("edad65", label = h4("13. Age 65 or older"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("edad65_13")
                             ),
                             column(4,
                                    sliderInput("edad17", label = h4("14. Age 17 or younger"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("edad17_14")
                             ),
                             column(4,
                                    sliderInput("disability", label = h4("15. Disability"), min = 0, 
                                                max = 3, value = 0),
                                    verbatimTextOutput("disability15")
                             )),
                         fluidRow(
                             column(4,
                                    sliderInput("single_house", label = h4("16. Single parent household"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("single_house16")
                             ),
                             column(4,
                                    sliderInput("ethnic", label = h4("17. Ethinic minority"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("ethnic17")
                             ),
                             column(4,
                                    sliderInput("multihouse", label = h4("18. Multi-unit housing"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("multihouse18")
                             )
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("nbpersons", label = h4("19. Crowded household"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("nbpersons19")
                             ),
                             column(4,
                                    sliderInput("vehicle", label = h4("20. Vehicle availability"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("vehicle20")
                             ),
                             column(4,
                                    sliderInput("groupq", label = h4("21. Group quarters"), min = 0, 
                                                max = 1, value = 0),
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
                                                max = 1, value = 0),
                                    verbatimTextOutput("speed19")
                             ),
                             column(4,
                                    sliderInput("case_qtine", label = h4("20. Proportion of cases identified among quarantined persons"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("case_qtine20")
                             ),
                             column(4, h4("21. Implementation of contact tracing"),
                                    verbatimTextOutput("contact_tracing21"))  
                         ),
                         fluidRow(
                             column(4,
                                    sliderInput("case_contact", label = h4("22. Proportion of cases identified from a contact list"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("case_contact22")
                             ),
                             column(4,
                                    sliderInput("prop_test", label = h4("23. Proportion of persons tested for every new COVID-19 case detected, in the last week"), min = 0, 
                                                max = 1, value = 0),
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
                                 sliderInput("prop_adhere", label = h4("26. Proportion of the population who adheres to mitigation measures"), min = 0, max = 1, value = 0),
                                 verbatimTextOutput("prop_adhere26")
                             )
                ), 
                         tableOutput("phsm_table_test"),
                         h4("Public Health and Social Measures Score"),
                tags$head(tags$style('h1 {color:red;}')),
                verbatimTextOutput("miti3"),
                tags$head(tags$style("#miti3{color: #0085b2;
                                 font-size: 16px;
                                 }"
                ))
            ),
            tabPanel("Epidemiology", 
                     h3("Epidemiology Statistics"),
                     verbatimTextOutput("contents5"),
                     plotOutput("plot1"),
                     plotOutput("plot2"),
                     tableOutput("contents2")),
            tabPanel("Score",
                     h3("Scores & Interpretation"),
                     fluidRow(
                         column(5, 
                                tableOutput("tbl")),
                         column(5, 
                                tags$img(src = "matrix1.png", height = 200, width = 400))
                     ),
                     #tableOutput("tbl"),
                     #tags$img(src = "matrix1.png", height = 200, width = 400),
                     tags$img(src = "matrix2.png", height = 450, width = 500))
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
    output$phsm_table_test <- renderTable({
        req(input$phsm_country)
        req(input$phsm_area)
        return(phsm_municipal())
    })
    
    df <- reactive({
        req(input$file1)
        x = file02()
        x[,1]<-as.Date(x[,1], "%d/%m/%Y")
        dfR <- estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))
        return(dfR)
    })
    
    
    output$plot1 <- renderPlot({
        plot(df(), what=c("incid"))
    })
    output$plot2 <- renderPlot({
        plot(df(), what=c("R"))
    })
    
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
    
    contexto02 <- reactive({
        if (is.null(municipal()$contexto02)) {
            return(input$water)
        }
        else {
            return(as.character(municipal()$contexto02))
        }
    })
    
    contexto03 <- reactive({
        if (is.null(municipal()$contexto03)) {
            return(input$occupation)
        }
        else {
            return(as.character(municipal()$contexto03))
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
    
    contexto05 <- reactive({
        if (is.null(municipal()$contexto05)) {
            return(input$publictrans)
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
    
    contexto09 <- reactive({
        if (is.null(municipal()$contexto09)) {
            return(input$stunted)
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
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto07())))
            return(disable("vac_children"))
        }
    })
    
    output$vac_elder08 <- renderPrint({
        if (is.null(municipal()$contexto08)) {
            return(writeLines("Please enter value."))
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
    
    output$contact_tracing21 <- renderPrint({
        if (phsm_municipal()$contact == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
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
                                       as.numeric(input$speed)*3 +
                                       as.numeric(input$case_qtine)*3 +
                                       as.numeric(phsm_municipal()$contact)*2 +
                                       as.numeric(input$case_contact)*3 +
                                       as.numeric(input$prop_test)*1 +
                                       as.numeric(phsm_municipal()$gathering)*2 +
                                       as.numeric(phsm_municipal()$disturbance)*2 +
                                       as.numeric(input$prop_adhere)*3
        )
        return(public_health_measures)
    })
    
    output$miti3 <- renderPrint({
        return(writeLines(as.character(public_health())))
    })
    
    Rt <- reactive({
        x <- df()$R$Mean
        return(round(x[length(x)],digits=2))
    })
    
    output$contents5 <- renderPrint({
        return(writeLines(c("The current reproductive number (R) is estimated to be", Rt())))
    })
        
    epi_transmission <- reactive({
        epi_trans <- (as.numeric(Rt())*2)
        return(epi_trans)
    })
    
    output$tbl <- renderTable({
        final_df <- data.frame(
            Category = c(
                "Context - Individual vulnerability",
                "Context - Social vulnerability",
                "Epidemiology - Transmission",
                "Epidemiology - Social vulnerability",
                "Healthcare Systems - Transmission",
                "Healthcare Systems - Mortality",
                "Public health and social measures"
            ),
            Score = c(individual(),
                      social(),
                      epi_transmission(),
                      individual(),
                      social(),
                      individual(),
                      social())
        )
        
        return(final_df)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
}

# Create Shiny app ----
shinyApp(ui, server)

