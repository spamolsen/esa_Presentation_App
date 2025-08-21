# Deployment Script for ESA Presentation App

# Install required packages if not already installed
required_pkgs <- c("rsconnect", "shiny", "shinythemes", "plotly", "mgcv", "ggplot2", "extrafont", "readxl")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

# Load required packages
library(rsconnect)

# Deploy the application
deployApp(
  appDir = ".",
  appName = "esa_presentation_app",
  appTitle = "ESA Presentation App",
  account = "sammyolsen",
  forceUpdate = TRUE,
  launch.browser = TRUE,
  lint = FALSE,
  server = "https://connect.posit.cloud",
  appFiles = c(
    "app.R",
    "ui.R",
    "server.R",
    "www/STEMinternshipLogo.png",
    "www/styles.css",
    "www/pt_hc.png",
    "www/pt_temp.png",
    "www/pt_ts.png",
    "www/sn_hc.png",
    "www/sn_temp.png",
    "www/sn_ts.png",
    "data/publication_models.RData"
  )
)
