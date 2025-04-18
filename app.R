# Main Application Loader
options(shiny.sanitize.errors = F)  # Show detailed errors in development
#options(future.debug = TRUE)  # Enable future debugging
#options(future.wait.interval = 0.5)  # Adjust wait interval for futures

# the shiny app appear to need files in alphabetical order
# and is loaded recursively, so top level will be loaded first
# therefore we need them to be at same level in folder structure
# Load packages FIRST
source("R/00_base/00_pkgs/00_pkgs.R")

# Then source other components
source("R/00_base/01_utils/00_utils.R")
source("R/00_base/01_utils/01_colors.R")

source("R/00_base/02_data/00_data.R")

# Shiny app files
source("R/01_shiny/00_modules/00_dataInit.R")
source("R/01_shiny/00_modules/01_chart_rtns.R")
source("R/01_shiny/00_modules/02_chart_hldgs.R")

source("R/01_shiny/01_main/00_ui.R")
source("R/01_shiny/01_main/01_server.R")

# Run the application
shinyApp(
    ui = app_ui,
    server = app_server
)
