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
library(readxl)
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
                                 ".csv",
                                 ".xlsx")),
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
                         uiOutput("selectfile3")

                         ),
                tabPanel("Individual Vunerability Index",
                         h3("Context"),
                         h4("Demographic and Socio-economic Information about the Neighborhood"),
                         fluidRow(
                             column(4,
                                    sliderInput("pop_dens", label = h4("1. Population density / Km²"), min = 0, 
                                                max = 4, value = 0),
                                    verbatimTextOutput("pop_dens01")
                
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
                tabPanel("Epidemiology", 
                         h3("Epidemiology Statistics"),
                         verbatimTextOutput("contents5"),
                         plotOutput("plot1"),
                         plotOutput("plot2"),
                         tableOutput("contents2"))
        ))))
        
    
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
    
    #output$table <- renderTable({ 
        #if(is.null(input$file1)){return()}
        #read.table(file=input$file1$datapath[input$file1$name==input$Select])
    #})
    
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
        read.xlsx(file=input$file1$datapath[input$file1$name==input$Select_test3],
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
    
    contexto01 <- reactive({
        if (is.null(municipal()$contexto01)) {
            return(input$pop_dens)
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
    
    
    
    # Shows the table for coded csv by selected municipal
    #output$contents <- renderTable({
        #if (is.null(df())) {
            #return(NULL)
        #}
        #else {
            #return(municipal())
        #}
    #})
    
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
    
    
    output$contents5 <- renderPrint({
        x <- df()$R$Mean
        return(writeLines(c("The current reproductive number (R) is estimated to be", round(x[length(x)],digits=2))))
    })
    
    
}

# Create Shiny app ----
shinyApp(ui, server)

