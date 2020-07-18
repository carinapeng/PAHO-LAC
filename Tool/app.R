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
#options(shiny.maxRequestSize=2650*1024^2)

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
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
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
                         withMathJax(includeMarkdown("/Users/carinapeng/Projects/Harvard-WHO/COVID19/Modeling-COVID19.md")),
                         h3(textOutput("upload")),
                         selectInput('mydropdown', label = 'Select', choices = 'No choices here yet'),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value"))),
                         # Display uploaded csv file
                         tableOutput("contents")
                ),
                tabPanel("Context",
                         h3("Demographic and Socio-economic Information about the Neighborhood"),
                         withMathJax(includeMarkdown("/Users/carinapeng/Dropbox/Harvard-WHO/Harvard-WHO/COVID19/policy.md")),
                         h3("Individual Vunerability Index"),
                         fluidRow(
                             column(4,
                                    sliderInput("pop_dens", label = h4("Population density / Km²"), min = 0, 
                                                max = 4, value = 0),
                                    verbatimTextOutput("pop_dens01")
                                    ),
                             column(4,
                                    sliderInput("water", label = h4("Availability of water and soap for hand washing inside the home"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("water02")
                                    ),
                             column(4,
                                    sliderInput("occupation", label = h4("Proportion of the population who is staffing an essential worker position"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("occupation03")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("workout", label = h4("Proportion of the population working outside the home"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("workout04")
                                    ),
                             column(4,
                                    sliderInput("publictrans", label = h4("Proportion of the population who uses public transport"), min = 0, 
                                                max = 2, value = 0),
                                    verbatimTextOutput("publictrans05")
                                    ),
                             column(4,
                                    sliderInput("comorbidity", label = h4("Proportion of persons with pre-existing comorbidities"), min = 0, 
                                                max = 3, value = 0),
                                    verbatimTextOutput("comorbidity06")
                                    )),
                         fluidRow(
                             column(4,
                                    sliderInput("vac_children", label = h4("Under- or non-vaccinated population: children younger than 1"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("vac_children07")
                                    ),
                             column(4,
                                    sliderInput("vac_elder", label = h4("Under- or non-vaccinated population: persons age 60 or older"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("vac_elder08")
                             ),
                             column(4,
                                    sliderInput("stunted", label = h4("Proportion of the population who is stunted"), min = 0, 
                                                max = 1, value = 0),
                                    verbatimTextOutput("stunted09")
                                    )
                         ),
                         h3("Risk Component: Context"),
                         verbatimTextOutput("context"),
                         verbatimTextOutput("context2"),
                         h3("Impact of Contact Reduction Policies on the Transmission"),
                         verbatimTextOutput("impactcontactreduction")
                )
            )
        )))
        
    
server <- function(input, output, session) { 
    
    csv <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    df <- reactive({
        req(input$file1)
        x = csv()
        return(x)
    })
    
    output$upload <- renderPrint({
        if (is.null(csv())) {
            return(NULL)
        }
        return(writeLines("Uploaded File"))
    })
    
    observeEvent(input$file1, {
        updateSelectInput(session, "mydropdown", label = "Select", choices = csv()$comuna)
    })
    
    municipal <- reactive({
        req(input$mydropdown)
        dfnew <- df() %>%
            filter(comuna == input$mydropdown)
        return(dfnew)
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
        return(writeLines(c("Individual Vunerability Score", individual())))
    })
    
    
    
    social <- reactive({
        social_vunerability <- (municipal()$contexto10*1 + 
                                    municipal()$contexto12*1 +
                                    municipal()$contexto13*2 +
                                    municipal()$contexto14*1 +
                                    municipal()$contexto17*2 +
                                    municipal()$contexto18*2 +
                                    municipal()$contexto19*3 +
                                    municipal()$contexto21*3
        )
        return(social_vunerability)
    })
    
    output$context2 <- renderPrint({
        return(writeLines(c("Social Vunerability Score", social())))
    })
    
    
    output$pctcontactreduction <- renderPrint({
        x <- df()$R$Mean
        Rt_observed <- x[length(x)]
        impact_contact_reduction <- (1 - Rt_observed / (input$R0 * contact()))*100
        return(writeLines(c("Percentage Reduction of COVID-19 Cases is estimated to be", round(impact_contact_reduction,digits=2), "Percent")))
        # return(, x[length(x)])
    })  
}

# Create Shiny app ----
shinyApp(ui, server)

