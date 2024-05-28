library(ggplot2)
library(shiny)

# Introduction sidebar
intro_sidebar <- sidebarPanel(
  h3("About the Project"),
  p("This project aims to explore..."),
  img(src = "your_image.png", height = "200px")  # Make sure to have an image in www directory
)

# Introduction main panel
intro_main_panel <- mainPanel(
  h4("Overview"),
  p("Here is a detailed description...")
)

# Sidebar for Analysis 1
analysis1_sidebar <- sidebarPanel(
  selectInput("selectVariable", "Choose a variable:", choices = colnames(mtcars)),
  sliderInput("slider1", "Number of Bins:", min = 1, max = 50, value = 10)
)

# Main panel for Analysis 1
analysis1_main_panel <- mainPanel(
  plotOutput("histPlot")
)

# Sidebar for Analysis 2 and 3 (shared if identical)
analysis23_sidebar <- sidebarPanel(
  helpText("Interactive visualization of data."),
  actionButton("btn", "Press me")
)

# Main panel for Analysis 2 and 3 (shared if identical)
analysis23_main_panel <- mainPanel(
  verbatimTextOutput("text")
)

# Conclusion main panel
conclusion_main_panel <- mainPanel(
  h3("Key Takeaways"),
  p("Here are the major insights derived from our analysis."),
  p("1. Insight one..."),
  p("2. Insight two..."),
  p("3. Insight three...")
)

# Introduction tab
intro_tab <- tabPanel(
  "Introduction",
  fluidPage(
    titlePanel("Introduction"),
    sidebarLayout(
      intro_sidebar,
      intro_main_panel
    )
  )
)

# Analysis 1 - DAVID
analysis1_tab <- tabPanel(
  "Analysis 1",
  fluidPage(
    titlePanel("Analysis of Variable 1"),
    sidebarLayout(
      analysis1_sidebar,
      analysis1_main_panel
    )
  )
)

# Analysis 2 - SAM
analysis2_tab <- tabPanel(
  "Analysis 2",
  fluidPage(
    titlePanel("Analysis of Variable 2"),
    sidebarLayout(
      analysis23_sidebar,
      analysis23_main_panel
    )
  )
)

# Analysis 3 - AARON
analysis3_tab <- tabPanel(
  "Analysis 3",
  fluidPage(
    titlePanel("Analysis of Variable 3"),
    sidebarLayout(
      analysis23_sidebar,
      analysis23_main_panel
    )
  )
)

# Conclusion tab
conclusion_tab <- tabPanel(
  "Conclusion",
  fluidPage(
    titlePanel("Summary of Findings"),
    conclusion_main_panel,
  )
)

# UI definition using navbarPage
ui <- navbarPage("Global Population Dynamics: The Role of Birth, Death, and Life Expectancy",
                 intro_tab,
                 analysis1_tab,
                 analysis2_tab,
                 analysis3_tab,
                 conclusion_tab
)



