# Load necessary libraries
library(shiny)
library(ggplot2)
library(ggmosaic)
library(gridExtra)

# Define the UI
ui <- fluidPage(
  titlePanel("Categorical Variable Plotting App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("varselect_x"),
      uiOutput("varselect_y"),
      uiOutput("varselect_fill"),
      selectInput("plot_type", "Plot Type",
                  choices = c("Bar Plot", "Stacked Bar Plot", "Mosaic Plot", "Heatmap", "Segmented Bar Plot")),
      actionButton("plot", "Plot"),
      downloadButton("download_plot", "Download Plot"),
      br(),
      uiOutput("summary_col_ui"),
      actionButton("summary_stats", "Show Summary Statistics")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary_stats_output")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive expression to read the uploaded dataset
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically generate UI for selecting variables based on dataset
  output$varselect_x <- renderUI({
    req(dataset())
    selectInput("xvar", "X Variable", names(dataset()))
  })
  
  output$varselect_y <- renderUI({
    req(dataset())
    selectInput("yvar", "Y Variable", names(dataset()))
  })
  
  output$varselect_fill <- renderUI({
    req(dataset())
    selectInput("fillvar", "Fill Variable", names(dataset()))
  })
  
  # Update summary column choices dynamically
  output$summary_col_ui <- renderUI({
    req(dataset())
    selectInput("summary_col", "Column for Summary Statistics", choices = names(dataset()))
  })
  
  # Reactive expression for the plot
  plot_data <- eventReactive(input$plot, {
    req(input$xvar, input$yvar, input$fillvar)
    
    plot <- switch(input$plot_type,
                   "Bar Plot" = {
                     ggplot(dataset(), aes_string(x = input$xvar)) +
                       geom_bar() +
                       labs(title = "Bar Plot",
                            x = input$xvar,
                            y = "Count")
                   },
                   "Stacked Bar Plot" = {
                     ggplot(dataset(), aes_string(x = input$xvar, fill = input$fillvar)) +
                       geom_bar(position = "stack") +
                       labs(title = "Stacked Bar Plot",
                            x = input$xvar,
                            y = "Count",
                            fill = input$fillvar)
                   },
                   "Mosaic Plot" = {
                     ggplot(dataset()) +
                       geom_mosaic(aes_string(x = paste0("product(", input$xvar, ")"), fill = input$fillvar)) +
                       labs(title = "Mosaic Plot",
                            x = input$xvar,
                            fill = input$fillvar)
                   },
                   "Heatmap" = {
                     ggplot(dataset(), aes_string(x = input$xvar, y = input$yvar, fill = input$fillvar)) +
                       geom_tile() +
                       labs(title = "Heatmap",
                            x = input$xvar,
                            y = input$yvar,
                            fill = input$fillvar)
                   },
                   "Segmented Bar Plot" = {
                     ggplot(dataset(), aes_string(x = input$xvar, fill = input$yvar)) +
                       geom_bar(position = "fill") +
                       labs(title = "Segmented Bar Plot",
                            x = input$xvar,
                            y = "Proportion",
                            fill = input$yvar)
                   }
    )
    list(type = "ggplot", plot = plot)
  })
  
  # Render the plot
  output$plot <- renderPlot({
    plot_info <- plot_data()
    print(plot_info$plot)
  })
  
  # Function to save the plot as an image file
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", gsub(" ", "_", input$plot_type), ".png", sep = "")
    },
    content = function(file) {
      plot_info <- plot_data()
      ggsave(file, plot = plot_info$plot, device = "png")
    }
  )
  
  # Display summary statistics for a selected column
  output$summary_stats_output <- renderPrint({
    req(input$summary_stats)
    req(input$summary_col)
    summary(dataset()[[input$summary_col]])
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
