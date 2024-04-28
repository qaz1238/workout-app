#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#CODE FOR PAGE ONE

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
}

# Complete Shiny app
shinyApp(ui = ui, server = server)
