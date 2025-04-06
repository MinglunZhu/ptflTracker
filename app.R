# Main Application Loader
options(shiny.sanitize.errors = F)  # Show detailed errors in development
#options(future.debug = TRUE)  # Enable future debugging
#options(future.wait.interval = 0.5)  # Adjust wait interval for futures

# Load packages FIRST
source("R/00_packages.R")

# Then source other components
source("R/00_utils.R")
source("R/01_data.R")

# Shiny app files
source("R/02_ui.R")

source("R/modules/dataInit.R")
source("R/03_server.R")

# Run the application
shinyApp(
    ui = app_ui,
    server = app_server
)
