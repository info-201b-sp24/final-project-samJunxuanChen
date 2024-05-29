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
