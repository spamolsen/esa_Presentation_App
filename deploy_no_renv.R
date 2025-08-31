library(rsconnect)

# Set account info
rsconnect::setAccountInfo(

)

# Specify exact package versions
pkg_versions <- list(
  shiny = "1.7.5",
  shinythemes = "1.2.0",
  plotly = "4.10.2",
  mgcv = "1.8-42",
  ggplot2 = "3.4.4",
  extrafont = "0.19"
)

# Create a requirements.R file
writeLines(
  sprintf("install.packages('%s', version = '%s', repos = 'https://cran.rstudio.com')", 
          names(pkg_versions), 
          pkg_versions),
  con = "requirements.R"
)

# Deploy the app
deployApp(
  appDir = ".",
  appName = "esa_presentation_app",
  appTitle = "ESA Presentation App",
  account = "sammyolsen",
  forceUpdate = TRUE,
  launch.browser = TRUE,
  lint = FALSE,
  envManagement = "packrat",
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
    "data/publication_models.RData",
    "requirements.R"
  )
)
