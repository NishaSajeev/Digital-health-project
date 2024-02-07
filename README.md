library(rsconnect)
library(shiny)
library(plotly)
library(readxl)
rsconnect::setAccountInfo(name='nishasajeevkumar-digitalhealth', token='26242581310081967895F57F228A017D', secret='5cnNObYul5roajmGUTGpCeN/SoyIAca4zJYoUu1V')

# Define the order of months
months_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel file (.xlsx format):"),
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Bar Plot", "Line Plot", "Combined")),
      selectInput("variable", "Select Variable:",
                  choices = c("No. of Cases", "No. of Deaths", "No. of Survivors",
                              "Affected above age 50", "Deaths above age 50",
                              "Mortality Rate (%)", "Survival Rate (%)"),
                  multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput("barplot"),
      plotlyOutput("lineplot"),
      plotlyOutput("combined_plot"),
      htmlOutput("html_render")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Read the uploaded Excel file
  dataset <- reactive({
    req(input$file)
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read_excel(inFile$datapath)
    df$Month <- factor(df$Month, levels = months_order, ordered = TRUE)
    return(df)
  })
  
  # Create bar plot
  output$barplot <- renderPlotly({
    req(dataset())
    req(input$variable)
    
    p <- plot_ly(data = dataset(), x = ~Month, type = 'bar') %>%
      layout(title = paste("Bar Plot of", input$variable),
             xaxis = list(title = "Month"),
             yaxis = list(title = input$variable))
    
    for (var in input$variable) {
      p <- add_trace(p, y = ~get(var), name = var)
    }
    
    p
  })
  
  # Create line plot
  output$lineplot <- renderPlotly({
    req(dataset())
    req(input$variable)
    
    p <- plot_ly(data = dataset(), x = ~Month, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = paste("Line Plot of", input$variable),
             xaxis = list(title = "Month"),
             yaxis = list(title = input$variable))
    
    for (var in input$variable) {
      p <- add_trace(p, y = ~get(var), name = var)
    }
    
    p
  })
  
  # Create combined plot
  output$combined_plot <- renderPlotly({
    req(dataset())
    req(input$variable)
    
    if (length(input$variable) == 0)
      return(NULL)
    
    if (input$plot_type == "Bar Plot") {
      p <- plot_ly(data = dataset(), x = ~Month, type = 'bar') %>%
        layout(title = "Combined Bar Plot",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Values"))
    } else if (input$plot_type == "Line Plot") {
      p <- plot_ly(data = dataset(), x = ~Month, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Combined Line Plot",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Values"))
    } else {
      p <- plot_ly(data = dataset(), x = ~Month) %>%
        layout(title = "Combined Plot",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Values"))
    }
    
    for (var in input$variable) {
      if (input$plot_type == "Bar Plot" | input$plot_type == "Combined") {
        p <- add_trace(p, y = ~get(var), name = var, type = 'bar')
      }
      if (input$plot_type == "Line Plot" | input$plot_type == "Combined") {
        p <- add_trace(p, y = ~get(var), name = var, type = 'scatter', mode = 'lines+markers')
      }
    }
    
    p
  })
  
  # Render HTML content
  output$html_render <- renderUI({
    HTML("<h3>NISHA SAJEEV KUMAR</h3>
         <p>Nisha Sajeev Kumar_Digital Health.</p>")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
