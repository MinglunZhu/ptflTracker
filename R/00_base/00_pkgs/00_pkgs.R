# Package Dependency Management
library(tidyverse)

library(progressr)

library(quantmod)
library(PerformanceAnalytics)
library(xts)

library(shiny)

library(shinyjs)
library(shinycssloaders)
library(shinydashboard)

library(htmlwidgets)
library(plotly)       
library(scales)
library(colorspace)
library(farver)
library(prismatic)

# Configure future to be compatible with shinyapps.io
#options(future.rng.onMisuse = "ignore")

#library(future)
#library(promises)
#library(reshape2)

# not working with shinyapps.io
# required_packages <- c(
#   "tidyverse",

#   #'stkVal',

#   "quantmod", 
#   "PerformanceAnalytics",
#   "xts",
  
#   "shiny",
#   "shinydashboard", 
#   "shinyjs",
#   "htmlwidgets",
#   "plotly",       
#   'scales',

#   "reshape2"
# )

# # Install if needed and load
# # if(!all(required_packages %in% installed.packages())) {
# #   install.packages(
# #     required_packages[!required_packages %in% installed.packages()],
# #     dependencies = TRUE
# #   )
# # }
# invisible(lapply(
#   required_packages, library, 
#   character.only = T
# ))