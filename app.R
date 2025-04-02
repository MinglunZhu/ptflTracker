# Main Application Loader

# Load packages FIRST
source("R/00_packages.R")

# Then source other components
source("R/00_utils.R")
source("R/01_data.R")
source("R/02_ui.R")
source("R/03_server.R")

# Run the application
shinyApp(
    ui = app_ui,
    server = app_server
)
