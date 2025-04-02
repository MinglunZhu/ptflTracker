# Package Dependency Management
required_packages <- c(
  "tidyverse",

  'stkVal',

  "quantmod", 
  "PerformanceAnalytics",
  "xts",
  
  "shiny",
  "shinydashboard", 
  "shinyjs",
  "htmlwidgets",
  "plotly",        # <-- Add this line
  'scales',

  "reshape2"
)

# Install if needed and load
# if(!all(required_packages %in% installed.packages())) {
#   install.packages(
#     required_packages[!required_packages %in% installed.packages()],
#     dependencies = TRUE
#   )
# }
invisible(lapply(
  required_packages, library, 
  character.only = T
))