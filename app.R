# install.packages("shiny")
# Load libraries
library("shiny")
library("dplyr")

# Source server and UI
source("app_server.R")
source("app_ui.R")

# Create a new `shinyApp()` using the loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)
