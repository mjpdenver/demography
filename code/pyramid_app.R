# app.R - Population Pyramid by Gender (5-year bins with proportion toggle)

# Increase max upload size
options(shiny.maxRequestSize = 100 * 1024^2)

# Load libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# UI definition
ui <- fluidPage(
    titlePanel("Population Pyramid by Gender (5-year bins)"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV file", accept = c(".csv")),
            uiOutput("year_ui"),
            uiOutput("county_ui"),
            numericInput("bin_width", "Age Bin Width (years)", value = 5, min = 1),
            checkboxInput("prop", "Show Proportions", FALSE)
        ),
        mainPanel(
            plotOutput("pyramidPlot", height = "600px")
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Reactive: read uploaded CSV
    data <- reactive({
        req(input$file)
        df <- read_csv(input$file$datapath, show_col_types = FALSE)
        df %>% mutate(age = as.integer(age))
    })
    
    # Year selector
    output$year_ui <- renderUI({
        df <- data()
        req(df$year)
        selectInput("year", "Year", choices = sort(unique(df$year)))
    })
    
    # County selector
    output$county_ui <- renderUI({
        df <- data()
        req(df$county)
        selectInput("county", "County", choices = sort(unique(df$county)))
    })
    
    # Prepare binned pyramid data
    plot_data <- reactive({
        df <- data()
        req(input$year, input$county, input$bin_width)
        df_sub <- df %>%
            filter(year == input$year, county == input$county)
        
        # Define bins and labels
        w <- input$bin_width
        max_age <- max(df_sub$age, na.rm = TRUE)
        breaks <- seq(0, ceiling((max_age + 1) / w) * w, by = w)
        labels <- paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
        
        # Assign to age groups
        df_binned <- df_sub %>%
            mutate(age_group = cut(age,
                                   breaks = breaks,
                                   right = FALSE,
                                   labels = labels,
                                   include.lowest = TRUE))
        
        # Aggregate counts
        df_pyr <- df_binned %>%
            group_by(age_group) %>%
            summarize(
                male_count   = sum(malepopulation,   na.rm = TRUE),
                female_count = sum(femalepopulation, na.rm = TRUE),
                .groups = 'drop'
            )
        
        # Compute proportions or use absolute counts
        total_pop <- sum(df_pyr$male_count + df_pyr$female_count)
        if (input$prop) {
            df_pyr <- df_pyr %>%
                mutate(
                    male   = -male_count / total_pop,
                    female =  female_count / total_pop
                )
        } else {
            df_pyr <- df_pyr %>%
                mutate(
                    male   = -male_count,
                    female =  female_count
                )
        }
        
        # Ensure ordered factor
        df_pyr %>%
            mutate(age_group = factor(age_group, levels = labels))
    })
    
    # Render plot
    output$pyramidPlot <- renderPlot({
        pd <- plot_data()
        req(nrow(pd) > 0)
        
        y_label <- if (input$prop) "Proportion" else "Population"
        y_scale <- if (input$prop) scale_y_continuous(labels = percent) else scale_y_continuous(labels = abs)
        
        ggplot(pd, aes(x = age_group)) +
            geom_bar(aes(y = male),   stat = "identity", fill = "steelblue") +
            geom_bar(aes(y = female), stat = "identity", fill = "tomato") +
            coord_flip() +
            y_scale +
            labs(
                title = paste("Population Pyramid for", input$county, input$year),
                x     = paste0("Age Group (", input$bin_width, "-year bins)"),
                y     = y_label
            ) +
            theme_minimal() +
            theme(
                axis.text.y = element_text(size = 8),
                plot.title  = element_text(hjust = 0.5)
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

