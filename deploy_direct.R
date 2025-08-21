library(rsconnect)

# Set account info
rsconnect::setAccountInfo(
  name='sammyolsen',
  token='3CB2E33B473DA1755BAB69CA89FEE262',
  secret='4VOaREiPAv4oR9GqFfGnXF+nibdwpVyM/kqTxkGI'
)

# Deploy the app using a different method
deployShinyApp(
  appDir = ".",
  appName = "esa_presentation_app",
  appTitle = "ESA Presentation App",
  account = "sammyolsen",
  forceUpdate = TRUE,
  launch.browser = TRUE,
  lint = FALSE
)
