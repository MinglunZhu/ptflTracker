# Check and deploy
library(rsconnect)

# First check for missing dependencies
rsconnect::appDependencies()

# Then deploy
rsconnect::deployApp(
  appName = "ptfltracker",
  appTitle = "Interactive Portfolio Tracker",
  forceUpdate = T
)