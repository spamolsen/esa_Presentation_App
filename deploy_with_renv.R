library(rsconnect)

# Set account info
rsconnect::setAccountInfo(
  name='sammyolsen',
  token='3CB2E33B473DA1755BAB69CA89FEE262',
  secret='4VOaREiPAv4oR9GqFfGnXF+nibdwpVyM/kqTxkGI'
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
