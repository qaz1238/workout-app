#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#COMBINATION!!


library(stringr)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)



# Set the file path
file_path <- "C:\\Users\\teres\\Desktop\\DataViz\\Activity 2\\dataset_garmin_activity (1).csv"

# Read the CSV file
data <- read.csv(file_path)

# UI for the combined page
ui <- fluidPage(
  titlePanel("Workout Progress"),
  # Use a navbarPage for navigation between pages
  navbarPage(
    "Overview",
    # First page content
    tabPanel("Overview",
             mainPanel(
               h3("Overall Result"),
               # Dropdown button to select user
               selectInput("select_user", "Select User:",
                           choices = unique(data$User),
                           selected = NULL),
               # Table to display activity data
               tableOutput("activity_table"),
               # Note directing users to the "Details" tab
               tags$div(
                 style = "margin-top: 20px;",
                 p("Please click on the Details tab above to see more.")
               )
             )
    ),
    # Second page content
    tabPanel("Details",
             titlePanel("Interactive Scatter Plot"),
             sidebarLayout(
               sidebarPanel(
                 # No selectInput for user since it's already selected in the first page
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
  )
)

# Server logic for the combined page
server <- function(input, output, session) {
  observeEvent(input$goto_details, {
    # When the button is clicked, navigate to the second page
    updateTabsetPanel(session, "tabs", selected = "Details")
  })
  
  output$activity_table <- renderTable({
    # Filter data based on the selected user
    user_data <- data[data$User == input$select_user, ]
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(
      Activity_Type = character(),
      Total_Distance_Covered = numeric(),
      Best_Speed_Covered = numeric(),
      Total_Time_Spent = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over unique activity types for the selected user
    unique_activities <- unique(user_data$Activity.Type)
    for (activity in unique_activities) {
      # Subset data for the current activity
      activity_data <- user_data[user_data$Activity.Type == activity, ]
      # Calculate total distance covered
      total_distance <- sum(activity_data$Distance)
      # Calculate best speed covered
      best_speed <- max(activity_data$Max.Speed.Best.Pace., na.rm = TRUE)
      # Calculate total time spent
      total_time <- sum(activity_data$Time)
      # Append results to the result data frame
      result_df <- rbind(result_df,
                         data.frame(Activity_Type = activity,
                                    Total_Distance_Covered = total_distance,
                                    Best_Speed_Covered = best_speed,
                                    Total_Time_Spent = total_time))
    }
    
    # Return the result data frame
    result_df
  })
  
  # Render scatter plot for second page
  output$scatter_plot <- renderPlotly({
    plot_data <- data[data$User == input$select_user, ]
    x_var <- input$x_variable
    y_var <- input$y_variable
    color_var <- input$color_variable
    
    plot_data <- plot_data[complete.cases(plot_data[, c(x_var, y_var, color_var)]), ]
    
    plot_ly(plot_data, x = ~get(x_var), y = ~get(y_var), color = ~get(color_var)) %>%
      add_markers() %>%
      layout(title = paste("Interactive Scatter Plot for", input$select_user),
             xaxis = list(title = input$x_variable),
             yaxis = list(title = input$y_variable))
  })
}

# Complete Shiny app
shinyApp(ui = ui, server = server)


