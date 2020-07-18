library(shiny)
library(data.table)

ui <- fluidPage(
    titlePanel("Multiple file uploads"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1",
                      label="Upload CSVs here",
                      multiple = TRUE),
            
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
        mainPanel(
            selectInput('mydropdown', label = 'Select', choices = 'No choices here yet')
        )
    )
)

server <- function(input, output, session) {
    
    file01 <- reactive({
        req(input$file1)
        read.csv(input$file1[[1, 'datapath']],
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    file02 <- reactive({
        req(input$file1)
        read.csv(input$file1[[2, 'datapath']],
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    
    observeEvent(input$file1, {
        updateSelectInput(session, "mydropdown", label = "Select", choices = file01()$comuna)
    })
 
}

shinyApp(ui = ui, server = server)
