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
                         downloadButton("downloadData", "Download Coded CSV File"),
                         h3(textOutput("contents1")),
                         selectInput('mydropdown', label = 'Select', choices = 'No choices here yet'),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value"))),
                         # Display uploaded csv file
                         tableOutput("contents")
                ),
                tabPanel("Graphs", 
                         plotOutput("contents3"),
                         plotOutput("contents4")
                ),
                tabPanel("Statistics", 
                         h3("Summary Statistics"),
                         verbatimTextOutput("contents5"),
                         tableOutput("contents2"),
                         downloadButton("downloadData2", "Download Summary Statistics of Transmission Rates")),
                tabPanel("Public Policy Analysis",
                         h3("Estimating the Impact of Public Health Measures on COVID-19 Transmission as Modeled in the CovidSIM Interface"),
                         withMathJax(includeMarkdown("/Users/carinapeng/Dropbox/Harvard-WHO/Harvard-WHO/COVID19/policy.md")),
                         h3("Period Duration Parameters (Days)"),
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
        )
        
    )
)

# Define server logic to read selected file ----
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
    
    observeEvent(input$file1, {
        updateSelectInput(session, "mydropdown", label = "Select", choices = csv()$comuna)
        
    })
    
    municipal <- reactive({
        req(input$mydropdown)
        dfnew <- df() %>%
            filter(comuna == input$mydropdown)
        return(dfnew)
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
    
    output$contents1 <- renderPrint({
        if (is.null(csv())) {
            return(NULL)
        }
        return(writeLines("Uploaded File"))
    })
    
    
    output$contents5 <- renderPrint({
        x <- df()$R$Mean
        return(writeLines(c("The current reproductive number (R) is estimated to be", round(x[length(x)],digits=2))))
        # return(, x[length(x)])
    })  
    
    individual <- reactive({
        individual_vunerability <- (municipal()$contexto02*1 + municipal()$contexto03*1 + municipal()$contexto04*1)
        return(individual_vunerability)
    })
    
    
    output$context <- renderPrint({
        y <- individual()
        # return(writeLines("Case Isolation Number Obtained", contact()))
        return(writeLines(c("Individual Vunerability Score",y)))
        # return(y)
        # return(impact_case_isolation)
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
        z <- social()
        return(writeLines(c("Social Vunerability Score",z)))
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

