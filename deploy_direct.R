library(rsconnect)

# Set account info
rsconnect::setAccountInfo(

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
