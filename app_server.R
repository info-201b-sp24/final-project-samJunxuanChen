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

  # Chart 2-SAM
  
  #Text description
  output$description <- renderText({
    paste("This analysis presents the top and bottom countries by average life expectancy from 2017-2021. By examining this data, we can gain insights into global health trends and disparities. Due to the nature of shared places and similar life expectancy values across countries, the top 5 countries might occasionally exceed 5 entries, same in bottom countries, providing a more comprehensive view of regions with higher life expectancies. This dataset has been processed to handle missing values effectively, ensuring the integrity of the analysis, but the missing data might be important for the analysis.")
  })
  
  data <- read.csv("DATA/Global Population Trends(2016-2022).csv")
  
  filtered_data <- reactive({
    # Replace "-" with NA
    data <- read.csv("DATA/Global Population Trends(2016-2022).csv", na.strings = "-")
    
    # Filter out NA values in Life.Expectancy column
    data <- na.omit(data, cols = "Life.Expectancy")
    
    # Convert Life.Expectancy column to numeric
    data$Life.Expectancy <- as.numeric(data$Life.Expectancy)
    
    # Return filtered data
    return(data)
  })
  
  # Render the bar plot
  output$life_expectancy_plot <- renderPlot({
    all_countries <- filtered_data()  # Reactive expression should be used here
    
    # Ensure filtered_data is a dataframe
    if (!is.data.frame(all_countries)) {
      return(NULL)  # Return NULL if filtered_data is not a dataframe
    }
    
    all_countries <- all_countries %>%
      group_by(Country) %>%
      summarize(avg_Life_Expectancy = round(mean(Life.Expectancy, na.rm = TRUE)))
    
    top_countries <- all_countries %>% top_n(input$num_top_countries, wt = avg_Life_Expectancy)
    bottom_countries <- all_countries %>% top_n(input$num_bottom_countries, wt = -avg_Life_Expectancy)
    
    combined_data <- bind_rows(top_countries %>% mutate(group = "Top"),
                               bottom_countries %>% mutate(group = "Bottom"))
    
    # Check if combined_data has rows
    print(combined_data)
    
    # Create bar plot with color and annotations
    ggplot(combined_data, aes(x = avg_Life_Expectancy, y = reorder(Country, avg_Life_Expectancy), fill = group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(avg_Life_Expectancy)), hjust = -0.2, color = "black", size = 3.5) +
      labs(title = paste("Top", input$num_top_countries, "and Bottom", input$num_bottom_countries, "Countries by Average Life Expectancy"),
           x = "Average Life Expectancy",
           y = "Country",
           fill = "Group") +
      scale_fill_manual(values = c("Top" = "blue", "Bottom" = "red")) +  # Set colors for bars
      theme_minimal() +
      theme(legend.position = "top")
  })
}

