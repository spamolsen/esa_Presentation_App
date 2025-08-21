# ESA GAM Models Presentation App

This Shiny application visualizes GAM model results for ESA presentation.

## Features
- Select between Pan Trap and Sweep Net models
- Choose predictor variables from the selected model
- View main effect plots
- View model diagnostics

## Deployment

To deploy this application using Posit Connect:

1. Ensure you have Posit Connect installed and properly configured
2. The application requires the following R packages:
   - shiny
   - shinythemes
   - mgcv
   - ggplot2

3. Place the following files in the application directory:
   - ui.R
   - server.R
   - app.R
   - www/ directory containing any static files
   - data/ directory containing model RData files

4. Deploy through Posit Connect's web interface or command line tools
