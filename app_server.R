library(ggplot2)
library(shiny)
library(readr) 
library(dplyr)

server <- function(input, output) {
  
  # Demographic Factors vs Population Density Plotting function
  output$densityPlot <- renderPlot({
    filtered_data <- global_population %>%
      filter(Year == input$yearSelect) %>%
      mutate(`Population Density` = as.numeric(gsub(",", "", `Population Density`))) %>%
      select(`Population Density`, one_of(input$factorSelect))
    
    if (input$factorSelect == "Fertility Rate") {
      ggplot(filtered_data, aes(x = `Population Density`, y = !!sym(input$factorSelect))) +
        geom_point() +
        scale_y_continuous(
          breaks = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 
          limits = c(0, 8)  
        ) +
        labs(x = "Population Density (per sq km)", y = input$factorSelect) +
        scale_x_continuous(limits = c(NA, 800)) + 
        theme_minimal()
    } else if (input$factorSelect == "Infant Mortality Rate") {
      ggplot(filtered_data, aes(x = `Population Density`, y = !!sym(input$factorSelect))) +
        geom_point() +
        scale_y_continuous(
          breaks = c(0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0),  
          limits = c(0, 21)
        ) +
        labs(x = "Population Density (per sq km)", y = input$factorSelect) +
        scale_x_continuous(limits = c(NA, 800)) + 
        theme_minimal()
    } else {
      ggplot(filtered_data, aes(x = `Population Density`, y = !!sym(input$factorSelect))) +
        geom_point() +
        labs(x = "Population Density (per sq km)", y = input$factorSelect) +
        scale_x_continuous(limits = c(NA, 800)) + 
        theme_minimal()
    }
  })
}

# Chart 3-Aaron
data <- read.csv("DATA/Global Population Trends(2016-2022).csv", stringsAsFactors = FALSE)


data$Total.Population <- as.numeric(gsub(",", "", data$Total.Population))
data$Urban.Population <- as.numeric(gsub(",", "", data$Urban.Population))

data <- data[data$Year != 2017,]

server <- function(input, output) {
  output$plot <- renderPlotly({
    
    filtered_data <- data[data$Year == input$year, ]
    
    p <- ggplot(filtered_data, aes(x = Total.Population, y = Urban.Population)) +
      geom_point() +
      scale_x_log10() +  
      scale_y_log10() +  
      labs(title = "Scatter Plot of Total Population vs. Urban Population",
           x = "Total Population (log scale)",
           y = "Urban Population (log scale)") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

shinyApp(ui = ui, server = server)

###
# Read data, ensuring strings are not converted to factors
data <- read.csv("DATA/Global Population Trends(2016-2022).csv", stringsAsFactors = FALSE)

# Print out unique values to understand what non-numeric characters might be present
print(unique(data$Total.Population))
print(unique(data$Urban.Population))

# Correct the use of regular expressions to remove non-numeric characters and convert strings to numeric
# Ensure commas are removed and other non-numeric characters are handled
data$Total.Population <- as.numeric(gsub("[^0-9.]", "", gsub(",", "", data$Total.Population)))
data$Urban.Population <- as.numeric(gsub("[^0-9.]", "", gsub(",", "", data$Urban.Population)))

# Filter out the year 2017
data <- data[data$Year != 2017,]

# Check the summary to see if there are any NAs and the general statistics
summary(data$Total.Population)
summary(data$Urban.Population)

# Optionally, handle NAs if needed
data <- na.omit(data)  # This removes all rows with any NA values

# Define server function for Shiny app
server <- function(input, output) {
  output$plot <- renderPlotly({
    req(data)  # Ensure data is available
    filtered_data <- data[data$Year == input$year, ]
    
    if(nrow(filtered_idata) == 0) {
      return(NULL)  # Exit if no data available for the selected year
    }
    
    # Generate a plot using ggplot2 and convert to Plotly for interactive features
    p <- ggplot(filtered_data, aes(x = Total.Population, y = Urban.Population)) +
      geom_point() +
      scale_x_log10() +  
      scale_y_log10() +  
      labs(title = "Scatter Plot of Total Population vs. Urban Population",
           x = "Total Population (log scale)",
           y = "Urban Population (log scale)") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}
