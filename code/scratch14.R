# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)

# Simulate a sample data frame with 10,000 observations (replace with your own data)


df <- read_csv("../data/places.csv", show_col_types = FALSE)

# Define the UI with three tabs
ui <- navbarPage("My Shiny App",
                 
                 # Tab 1: Text Content
                 tabPanel("Text",
                          fluidPage(
                              h1("Welcome to My Shiny App!"),
                              p("This app displays a map with records and a weekly aggregated time series plot color coded by category.")
                          )
                 ),
                 
                 # Tab 2: Map
                 tabPanel("Map",
                          fluidPage(
                              leafletOutput("map", height = 500)
                          )
                 ),
                 
                 # Tab 3: Time Series Plot
                 tabPanel("Time Series",
                          fluidPage(
                              plotOutput("timeSeriesPlot", height = 500)
                          )
                 )
)

# Define the server logic
server <- function(input, output, session) {
    
    # Render the leaflet map with markers for each record
    output$map <- renderLeaflet({
        leaflet(df) %>%
            addTiles() %>%
            addMarkers(
                lng = ~lng, 
                lat = ~lat,
                popup = ~paste("<strong>", name, "</strong><br>",
                               "Category:", category, "<br>",
                               "Date:", date)
            ) %>%
            setView(lng = -105, lat = 39.75, zoom = 10)
    })
    
    # Render the time series plot aggregated by week and color coded by category
    output$timeSeriesPlot <- renderPlot({
        
        # Group the data by week (floor_date rounds down to the start of the week) and category
        df_summary <- df %>%
            group_by(week = floor_date(date, unit = "week"), category) %>%
            summarize(count = n(), .groups = "drop")
        
        # Create the time series plot with ggplot2
        ggplot(df_summary, aes(x = week, y = count, color = category)) +
            geom_line(size = 1) +
            geom_point() +
            labs(title = "Weekly Time Series Plot by Category",
                 x = "Week",
                 y = "Count") +
            theme_minimal() +
            theme(legend.title = element_blank())
    })
}

# Run the application
shinyApp(ui, server)
