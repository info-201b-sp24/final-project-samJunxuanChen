library(ggplot2)
library(shiny)
library(readr)
library(shinythemes)
# Load this library if you prefer read_csv

# Store data
global_population <- read_csv("DATA/Global Population Trends(2016-2022).csv") %>%
  mutate(
    `Population Density` = as.numeric(gsub("[^0-9.]", "", `Population Density`)),
    `Fertility Rate` = as.numeric(gsub("[^0-9.]", "", `Fertility Rate`)),
    `Infant Mortality Rate` = as.numeric(gsub("[^0-9.]", "", `Infant Mortality Rate`))
  )

# Introduction sidebar
intro_sidebar <- sidebarPanel(
  h3("About the Project"),
  p("Our project aims to unravel the complexity of population dynamics by investigating the relationship between different demographic factors, such as life expectancy, birth rate, and death rate, and overall population size."),
  img(style="width: 100%;",src = "population_image.png"),
  img(style="width: 100%;",src = "population_image2.jpg"),
)

# Introduction main panel
intro_main_panel <- mainPanel(
  h4( style="font-size:25px; font-weight:bold;", "Overview"),
  p("These factors are critical to understanding the broader consequences of demographic shifts for societal structures and individual well-being. Our project raises several research questions, including:"),
  p("- How does life expectancy correlate with changes in population size across different regions?"),
  p("- What impact do variations in birth and death rates have on the population size of different countries?"),
  p(style="margin-bottom: 30px", "- Which factors, life expectancy, birth rate, death rate, or fertility rate contribute most to population?"),
  h4(style="font-size:25px; font-weight:bold;", "Motivations"),
  p(style="margin-bottom: 30px","The motivations behind these research questions are rooted in understanding how demographic shifts can influence a wide range of societal and economic outcomes. Through these questions we can assess how increases in life expectancy influence regional population structures and the associated economic and social planning needs; we can analyze how changes in birth and death rates drive population growth or decline, informing resource allocation, infrastructure development, and economic strategies; and we can identify the most impactful demographic factors on population dynamics to determine the effectiveness of different societal factors such as healthcare, education, and economic planning."),
  h4(style="font-size:25px; font-weight:bold;", "Implications"),
  p(style="margin-bottom: 30px","If the research in our project succeeds in answering the questions posed, we believe that technicians, designers, and policymakers will be greatly affected. First, for technicians, understanding the impact of demographic factors on population dynamics can help them build models for prediction in fields such as public health, urban planning, and resource management. For designers, they can use this research to create more adaptable and sustainable living environments to meet changing demographic characteristics, such as the aging of the population that many countries in the world are now facing. Or the problem of rapid urbanization. For policymakers, they will have very comprehensive data to analyze issues such as education, health care and pensions, and can help them solve social phenomena such as overpopulation, underpopulation or an aging workforce. This multidisciplinary approach ensures that social infrastructure develops in step with demographic trends, promoting stability and prosperity."),
  h4(style="font-size:25px; font-weight:bold;", "The Data"),
  p(style="margin-bottom: 30px","The data was found on Kaggle. Here is the link to the data source: ", tags$a(href="https://www.kaggle.com/datasets/alitaqi000/global-population-trends2016-2022", "Global Population Trends(2016-2022)", target="_blank"), ". The data was collected by Syed Ali Taqi. The data compiled data from Macrotrends, ", tags$a(href="https://www.macrotrends.net/", "Macrotrends Official Website", target="_blank"), ". Macrotrends is a company that provides a standard stock screener and their data collection methodology is to take information from government records and integrate that data with economic data, financial market data, company expert judgment and data. The data using in the project are cited by Syed Ali Taqi from Macrotrends’ data integration of Population. There are 1074 observations (rows) in the dataset. There are 12 features (columns) in the dataset. In conducting the analysis, conclusions drawn from the data may have ethical implications, especially if the conclusions contribute to the perpetuation of stereotypes or stigmatization of certain groups of the population. It is important to consider the impact that the data may have on different communities and whether the use of the data is consistent with principles of social justice and equity. A limiting factor may be the accuracy and reliability of the data, as demographic data collection methods may differ across countries and regions, leading to inconsistencies and potential bias. In addition, missing or incomplete data may affect the comprehensiveness of the analysis. In addition, demographic indicators may not fully capture the complexity of population dynamics and certain population groups like minors may be underrepresented in the data."),
  h4(style="font-size:25px; font-weight:bold;", "Limitations & Challenges"),
  p(style="margin-bottom: 30px","In our opinion, a significant limitation of our project is the challenge of data heterogeneity and standardization. Demographic data may be collected using different methods in different countries and regions. This can lead to inconsistencies in comparative analyses, for example, the indicators measuring birth and death rates may differ, or the definition of “urban” versus “rural” may differ. Differences in these indicators will affect the accuracy of the analysis. In addition, demographic characteristics change rapidly due to factors such as immigration, policy changes, or major economic shifts. This means that not only can the data we use be inconsistent, but it can also quickly become outdated. Therefore, any conclusions drawn must be considered within the context of limitations and are subject to the possibility of bias and error. The project will require rigorous data validation methods and adaptive approaches to accommodate and reconcile these differences to ensure findings are reliable and reflect true population dynamics."),
)

# Demographic Factors vs Population Density Comparison
analysis1_tab <- tabPanel(
  "Population Dynamics",
  fluidPage(
    titlePanel("Demographic Factors vs. Population Density"),
    sidebarLayout(
      sidebarPanel(
        selectInput("yearSelect", "Select Year:", choices = unique(global_population$Year)),
        selectInput("factorSelect", "Select Demographic Factor:", choices = c("Death Rate" = "Death Rate", "Birth Rate" = "Birth Rate", "Fertility Rate" = "Fertility Rate", "Infant Mortality Rate" = "Infant Mortality Rate")),
        width = 4
      ),
      mainPanel(
        plotOutput("densityPlot", width ="80%", height = 600),
        p(style="margin-top: 30px", "The charts presented through the selections aim to explore the relationship between population density and various demographic factors such as death rate, fertility rate, infant mortality rate, and birth rate across different countries through 2017-2021. These visual analyses are crucial in understanding how densely populated areas differ in demographic behaviors and trends, which can inform policy decisions, resource allocation, and public health initiatives."),
        p(style="margin-top: 10px", "Observations suggest that denser populations may show different patterns, such as potentially lower fertility rates due to urban lifestyle factors or varied infant mortality rates that could indicate the level of healthcare accessibility. However, this is not always the case, as the plots are pretty scattered around overall through all charts without a specific correlation. For example, higher population densities do not necessarily correspond to higher birth rates, as one may assume. This shows that population is not very heavily reliant on these factors; suggesting influences from other various factors impact population density more significantly"),
        p(style="margin-top: 10px", "Such data is crucial for policymakers and planners to prioritize investments, develop targeted health and education programs, and effectively manage resources to address the specific needs of densely versus sparsely populated areas, ultimately guiding strategic decisions toward sustainable development and improved quality of life across different demographics."),
      )
    )
  )
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
    theme = shinytheme("united"),
    titlePanel("Introduction to Global Population Dynamics"),
    sidebarLayout(
      intro_sidebar,
      intro_main_panel
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



