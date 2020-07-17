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
#options(shiny.maxRequestSize=2650*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
    
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
                         sliderInput("pop_dens", label = h5("Population density / Km²"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("pop_dens01"),
                         
                         sliderInput("water", label = h5("Availability of water and soap for hand washing inside the home"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("water02"),
                         
                         sliderInput("occupation", label = h5("Proportion of the population who is staffing an essential worker position"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("occupation03"),
                         
                         sliderInput("workout", label = h5("Proportion of the population working outside the home"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("workout04"),
                         
                         sliderInput("publictrans", label = h5("Proportion of the population who uses public transport"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("publictrans05"),
                         
                         sliderInput("comorbidity", label = h5("Proportion of persons with pre-existing comorbidities"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("comorbidity06"),
                         
                         sliderInput("vac_children", label = h5("Under- or non-vaccinated population: children younger than 1"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("vac_children07"),
                         
                         sliderInput("vac_elder", label = h5("Under- or non-vaccinated population: persons age 60 or older"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("vac_elder08"),
                         
                         sliderInput("stunted", label = h5("Proportion of the population who is stunted"), min = 0, 
                                     max = 31, value = 2),
                         verbatimTextOutput("stunted09"),
                         
                         fluidRow(
                             column(3,
                                    
                                    # Copy the line below to make a slider bar 
                                    sliderInput("Dp", label = h4("Prodromal Period (Days)"), min = 0, 
                                                max = 31, value = 2)
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("Di", label = h4("Early Infective (Days)"), min = 0, 
                                                max = 31, value = 5)
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("Dl", label = h4("Late Infective (Days)"), min = 0, 
                                                max = 31, value = 7)
                             )
                         ),
                         h3("Relative Contagiousness Parameters (%)"),
                         fluidRow(
                             column(3,
                                    
                                    # Copy the line below to make a slider bar 
                                    sliderInput("Cp", label = h4("Prodromal (%)"), min = 0, 
                                                max = 1, value = 1)
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("Cl", label = h4("Early Infective (%)"), min = 0, 
                                                max = 1, value = 0.05)
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("R0", label = h4("Initial Rate of Infection"), min = 0, 
                                                max = 10, value = 3.7, step=0.1)
                             )
                         ),
                         h3("Transmission Parameters"),
                         fluidRow(
                             column(3,
                                    
                                    # Copy the line below to make a slider bar 
                                    sliderInput("Fsick", label = h4("Infections Which Will Lead to Sickness (%)"), min = 0, 
                                                max = 1, value = 0.67),
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("Fiso", label = h4("Probability Sick Person is Isolated (%)"), min = 0, 
                                                max = 1, value = 0.5)
                             ),
                             column(3,
                                    
                                    # Copy the line below to make a slider range 
                                    sliderInput("Phome", label = h4("Contact Reduction for Isolated Cases (%)"), min = 0, 
                                                max = 1, value = 0.75)
                             ),
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
    
    output$upload <- renderPrint({
        if (is.null(csv())) {
            return(NULL)
        }
        return(writeLines("Uploaded File"))
    })
    
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
            return(municipal()$contexto01)
        }
    })
    
    contexto02 <- reactive({
        if (is.null(municipal()$contexto02)) {
            return(input$water)
        }
        else {
            return(municipal()$contexto02)
        }
    })
    
    contexto03 <- reactive({
        if (is.null(municipal()$contexto03)) {
            return(input$Fiso)
        }
        else {
            return(municipal()$contexto03)
        }
    })
    
    contexto04 <- reactive({
        if (is.null(municipal()$contexto04)) {
            return(input$Fiso)
        }
        else {
            return(municipal()$contexto04)
        }
    })
    
    
    # Can delete later
    output$contents <- renderTable({
        
        if (is.null(df())) {
            return(NULL)
        }
        
        else {
            return(municipal())
        }
        
    })
    
    output$pop_dens01 <- renderPrint({
        return(writeLines(c("Population density / Km²", contexto01())))
    })
    
    output$water02 <- renderPrint({
        return(writeLines(c("Availability of water and soap for hand washing inside the home", contexto02())))
    })
    
    output$occupation03 <- renderPrint({
        return(writeLines(c("Proportion of the population who is staffing an essential worker position", contexto03())))
    })
    
    output$workout04 <- renderPrint({
        return(writeLines(c("Proportion of the population working outside the home", contexto04())))
    })
    
    output$publictrans05 <- renderPrint({
        return(writeLines(c("Proportion of the population who uses public transport", contexto05())))
    })
    
    output$comorbidity06 <- renderPrint({
        return(writeLines(c("Proportion of persons with pre-existing comorbidities", contexto06())))
    })
    
    output$vac_children07 <- renderPrint({
        return(writeLines(c("Under- or non-vaccinated population: children younger than 1", contexto07())))
    })
    
    output$vac_elder08 <- renderPrint({
        return(writeLines(c("Under- or non-vaccinated population: persons age 60 or older", contexto08())))
    })
    
    output$stunted09 <- renderPrint({
        return(writeLines(c("Proportion of the population who is stunted", contexto09())))
    })
    
    

    individual <- reactive({
        individual_vunerability <- (as.numeric(contexto01())*2 + 
                                        as.numeric(contexto02())*1 + 
                                        as.numeric(contexto03())*1 + 
                                        as.numeric(contexto04())*1)
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

