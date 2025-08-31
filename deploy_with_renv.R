library(rsconnect)

# Set account info
rsconnect::setAccountInfo(
)

# Initialize renv if not already initialized
if (!file.exists("renv.lock")) {
  library(renv)
  renv::init()
  renv::snapshot()
}

# Deploy the app
deployApp(
  appDir = ".",
  appName = "esa_presentation_app",
  appTitle = "ESA Presentation App",
  account = "sammyolsen",
  forceUpdate = TRUE,
  launch.browser = TRUE,
  lint = FALSE,
  envManagement = "renv"
)
