
# app.R - Population Pyramid by Gender (5-year bins with baseline overlay and accurate proportions)

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
    titlePanel("Population Pyramid by Gender"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV file", accept = c(".csv")),
            uiOutput("year_ui"),
            uiOutput("baseline_ui"),
            uiOutput("county_ui"),
            numericInput("bin_width", "Age Bin Width (years)", value = 5, min = 1),
            checkboxInput("prop", "Show Proportions of Total Population", FALSE)
        ),
        mainPanel(
            plotOutput("pyramidPlot", height = "600px")
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Read and prepare data
    df_all <- reactive({
        req(input$file)
        read_csv(input$file$datapath, show_col_types = FALSE) %>%
            mutate(age = as.integer(age))
    })
    
    # UI selectors
    output$year_ui <- renderUI({
        df <- df_all()
        req(df$year)
        selectInput("year", "Select Year:", sort(unique(df$year)))
    })
    output$baseline_ui <- renderUI({
        df <- df_all()
        req(df$year)
        selectInput("baseline_year", "Baseline Year (lines):", sort(unique(df$year)), selected = min(df$year))
    })
    output$county_ui <- renderUI({
        df <- df_all()
        req(df$county)
        selectInput("county", "Select County:", sort(unique(df$county)))
    })
    
    # Function to bin ages and aggregate counts or proportions
    bin_aggregate <- function(df, bin_width, prop) {
        # Bin definitions
        w <- bin_width
        max_age <- max(df$age, na.rm = TRUE)
        breaks <- seq(0, ceiling((max_age + 1) / w) * w, by = w)
        labels <- paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
        
        # Aggregate raw counts
        agg <- df %>%
            mutate(age_group = cut(age, breaks = breaks, right = FALSE,
                                   labels = labels, include.lowest = TRUE)) %>%
            group_by(age_group) %>%
            summarize(
                male_count   = sum(malepopulation,   na.rm = TRUE),
                female_count = sum(femalepopulation, na.rm = TRUE),
                .groups = 'drop'
            )
        
        # Total population for proportions
        total_pop <- sum(agg$male_count + agg$female_count, na.rm = TRUE)
        
        # Compute display values
        agg %>%
            mutate(
                male   = if (prop) -male_count / total_pop else -male_count,
                female = if (prop)  female_count / total_pop else  female_count,
                age_group = factor(age_group, levels = labels)
            )
    }
    
    # Prepare main and baseline data
    plot_data <- reactive({
        df <- df_all() %>% filter(year == input$year, county == input$county)
        req(nrow(df) > 0)
        bin_aggregate(df, input$bin_width, input$prop)
    })
    
    baseline_data <- reactive({
        df <- df_all() %>% filter(year == input$baseline_year, county == input$county)
        req(nrow(df) > 0)
        bin_aggregate(df, input$bin_width, input$prop)
    })
    
    # Render plot
    output$pyramidPlot <- renderPlot({
        pd <- plot_data()
        bl <- baseline_data()
        
        # Axis labels and scales
        y_label <- if (input$prop) "Proportion of Total Population" else "Population"
        y_scale <- if (input$prop) scale_y_continuous(labels = percent) else scale_y_continuous(labels = abs)
        
        ggplot(pd, aes(x = age_group)) +
            # Main bars
            geom_bar(aes(y = male),   stat = "identity", fill = "steelblue") +
            geom_bar(aes(y = female), stat = "identity", fill = "tomato") +
            # Baseline overlay lines
            geom_line(data = bl, aes(x = age_group, y = male, color = "Baseline Male", group = 1),
                      linetype = "dashed", size = 1) +
            geom_line(data = bl, aes(x = age_group, y = female, color = "Baseline Female", group = 1),
                      linetype = "dashed", size = 1) +
            coord_flip() +
            y_scale +
            scale_color_manual(name = "Baseline",
                               values = c("Baseline Male" = "darkblue", "Baseline Female" = "darkred")) +
            labs(
                title = paste("Population Pyramid for", input$county, input$year),
                subtitle = paste("Baseline Year:", input$baseline_year),
                x = paste0("Age Group (", input$bin_width, "-year bins)"),
                y = y_label
            ) +
            theme_minimal() +
            theme(
                axis.text.y   = element_text(size = 8),
                plot.title    = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5)
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

