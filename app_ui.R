library(ggplot2)
library(shiny)
library(readr)
library(shinythemes)

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

# Analysis 2 - Life Expectancy Variation
analysis2_tab <- tabPanel(
  "Life Expectancy Comparison",
  fluidPage(
    titlePanel("Life Expectancy Analysis"),
    sidebarLayout(
      sidebarPanel(
        numericInput("num_top_countries", "Top Countries:", value = 5, min = 1, max = 100),
        numericInput("num_bottom_countries", "Bottom Countries:", value = 5, min = 1, max = 100)
      ),
      mainPanel(
        textOutput("description"),
        plotOutput("life_expectancy_plot")
      )
    )
  )
)


# Conclusion main panel
conclusion_main_panel <- mainPanel(
  p("First, our analysis of average life expectancy in different countries over the period 2018-2021 shows that the twenty countries with the lowest life expectancy are all located on the African continent, where none of these countries has a life expectancy of more than 60 years. This finding suggests that these countries need to improve economic stability, quality of life, local government strategic decision-making, resource allocation, and social planning. In contrast, the 20 countries with the highest life expectancy are located outside the African continent, with 14 in Europe, three in Asia, two in Oceania, and one in the Americas. These countries have great education, health, and social systems. Resource allocation and social planning play a crucial role in determining life expectancy.
"),
  p("Second, our analysis shows no simple positive correlation between population density and birth rates. Although some high-density countries have higher birth rates, the overall distribution of the data suggests that a significant variability exists in birth rates, regardless of the country's population density. This suggests a greater complexity and diversity of factors affecting population density, including economic, cultural, and policy influences. This finding is of great value to policymakers when making health care and urban planning decisions.
"),
  p("Third, the analysis of the relationship between the total population and the urban population reveals a strong positive correlation. As a country's total population increases, its urban population also tends to increase. This finding implies that more populous countries usually have larger urban populations, possibly due to better economic opportunities, more developed infrastructure, or more active urban growth policies. However, we also noticed some deviations from this trend, with some countries having smaller urban populations than expected for their overall population. These deviations might result from geographical constraints, different stages of economic development, or specific national policies affecting urban development.
"),
  p("These findings contribute significantly to our understanding of global population dynamics and emphasize the importance of resource allocation and social planning in increasing life expectancy and promoting urbanization. The most important insight is that equitable resource allocation and effective social planning can significantly improve a country's average life expectancy and quality of life. This insight is crucial for policymakers, urban planners, and socioeconomic researchers. In a broader perspective, these insights are not limited to improving different countries' social and economic conditions, but can also have a global positive impact. For example, international cooperation and resource sharing effectively address global health and development inequalities. Developed countries can support developing countries' health and education systems through knowledge sharing, thereby improving overall global health.
"),
  p("In addition, we should consider the critical role of scientific and technological progress and innovation in increasing life expectancy and promoting urbanization. For example, public health interventions and resource allocation can be made with greater precision using artificial intelligence technology, which has been in the public eye a lot lately, to maximize policy impact. Through real-time monitoring and data analysis, governments and related organizations can respond more quickly to demographic changes and health needs, avoiding wasted resources and unfair distribution."),
  p("In summary, our analysis reveals key issues in current global population dynamics and proposes several innovative solutions. We promise to realize a more equitable and prosperous global society through international cooperation and scientific and technological innovation.
"),
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

# Analysis 3 - AARON
analysis3_tab <- tabPanel(
  "Total vs. Urban Population",
  fluidPage(
    titlePanel("Analysis of Total vs. Urban Population"),
    sidebarLayout(
      sidebarPanel(
        selectInput("yearSelect3", "Select Year:", choices = unique(global_population$Year[global_population$Year != 2017])),
        width = 4 
      ),
      mainPanel(
        plotOutput("populationPlot", width = "80%", height = 600),
        p(style="margin-top: 30px", "This plot illustrates the relationship between the total population and the urban population across different countries for the selected year. Understanding this relationship helps to analyze urbanization trends and their impacts on resource allocation and urban planning."),
        p(style="margin-top: 10px", "By examining how urban population scales with total population, we can identify regions that are undergoing rapid urbanization and might face sustainability challenges. This analysis also helps to understand the spatial distribution of population and its implications for economic development and infrastructure."),
        p(style="margin-top: 10px", "Note: The scatter plot provided reflects data variability and may help in identifying patterns of urban sprawl or consolidation characteristic of different regions around the world. Such insights are invaluable for policymakers and urban planners who aim to enhance the quality of life and ensure balanced regional development.")
      )
    )
  )
)

# Conclusion tab
conclusion_tab <- tabPanel(
  "Conclusion",
  fluidPage(
    titlePanel("Summary of Findings"),
    fluidRow(
      column(7, img(style="width: 100%;", src = "population_image3.png")),
    ),
    conclusion_main_panel
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



