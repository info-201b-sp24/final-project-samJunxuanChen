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
data <- read.csv("~/Desktop/info/GlobalPopulationTrends(2016-2022)2.csv", stringsAsFactors = FALSE)
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