#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#TABLE FOR PAGE TWO

library(stringr)


# Set the file path
file_path <- "C:\\Users\\teres\\Desktop\\DataViz\\Activity 2\\dataset_garmin_activity (1).csv"

# Read the CSV file
data <- read.csv(file_path)


# Display the first few rows of the data
head(data)


library(shiny)
# UI for the first page (overview)
ui <- fluidPage(
  titlePanel("Workout Overview"),
  mainPanel(
    h3("Overall Result"),
    # Dropdown button to select user
    selectInput("select_user", "Select User:",
                choices = unique(data$User),
                selected = NULL),
    # Table to display activity data
    tableOutput("activity_table"),
    # Add button to navigate to the second page
    actionButton("goto_details", "View Details")
  )
)

# Server logic for the first page
server <- function(input, output, session) {
  observeEvent(input$goto_details, {
    # When the button is clicked, navigate to the second page
    updateTabsetPanel(session, "tabs", selected = "details")
  })

  output$activity_table <- renderTable({
    # Filter data based on the selected user
    user_data <- data[data$User == input$select_user, ]
    # Return the filtered data frame
    user_data
  })
}

# Complete Shiny app
shinyApp(ui = ui, server = server)

.
.

.
.
.
.
..
.
.

#PAGE 2!!!!!!

library(stringr)
library(shiny)
library(dplyr)
library(ggplot2)

# Set the file path
file_path <- "C:\\Users\\teres\\Desktop\\DataViz\\Activity 2\\dataset_garmin_activity (1).csv"

# Read the CSV file
data <- read.csv(file_path)


# Display the first few rows of the data
head(data)


# UI
ui <- fluidPage(
  titlePanel("Distance Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("user_selection", "Select User:", choices = c("A", "B", "C", "D")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = NULL, end = NULL,
                     format = "dd M yy")  # Specify the date format here
    ),
    mainPanel(
      plotOutput("distance_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Reactive expression to filter data based on selected date range and user
  filtered_data <- reactive({
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    user <- input$user_selection
    filtered <- filter(data, User == user & as.Date(Date.Start, format = "%d %b %Y") >= start_date &
                         as.Date(Date.Start, format = "%d %b %Y") <= end_date)
    return(filtered)  # Explicitly return the filtered data
  })

  # Render the line chart
  output$distance_plot <- renderPlot({
    req(filtered_data())  # Ensure filtered_data is not NULL

    ggplot(filtered_data(), aes(x = Time, y = Distance)) +
      geom_line() +
      labs(x = "Time", y = "Distance Covered") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)









