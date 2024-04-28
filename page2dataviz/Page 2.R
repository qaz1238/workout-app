#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#PAGE 2!!!!!!

library(stringr)
library(shiny)
library(dplyr)
library(ggplot2)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Set the file path
file_path <- "C:\\Users\\teres\\Desktop\\DataViz\\Activity 2\\dataset_garmin_activity (1).csv"

# Read the CSV file
data <- read.csv(file_path)

head(data)

# UI
ui <- fluidPage(
  titlePanel("Interactive Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("user_selection", "Select User:",
                  choices = unique(data$User)),
      selectInput("x_variable", "Select X Variable:",
                  choices = c("Distance", "Elevation.Gain", "Calories")),
      selectInput("y_variable", "Select Y Variable:",
                  choices = c("Distance", "Elevation.Gain", "Calories")),
      selectInput("color_variable", "Select Color Variable:",
                  choices = c("Activity.Type", "Day.Start"))
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$scatter_plot <- renderPlotly({
    plot_data <- subset(data, User == input$user_selection)
    x_var <- input$x_variable
    y_var <- input$y_variable
    color_var <- input$color_variable

    plot_data <- plot_data[complete.cases(plot_data[, c(x_var, y_var, color_var)]), ]

    plot_ly(plot_data, x = ~get(x_var), y = ~get(y_var), color = ~get(color_var)) %>%
      add_markers() %>%
      layout(title = paste("Interactive Scatter Plot for", input$user_selection),
             xaxis = list(title = input$x_variable),
             yaxis = list(title = input$y_variable))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
