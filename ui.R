library(shiny)
library(shinythemes)
library(plotly)

ui <- fluidPage(
  # Include our custom CSS
  includeCSS("www/styles.css"),
  
  # Custom header
  div(id = "header",
      div(style = "text-align: right;", h1("ESA 2025 - Sammy Olsen")),
      img(src = "STEMinternshipLogo.png", id = "header-logo", alt = "STEM Internship Logo")),
  
  # Main panel for displaying outputs
  mainPanel(
    tabsetPanel(
      tabPanel("Plots", 
               fluidRow(
                 column(3,
                   selectInput(
                     "model_type",
                     "Select Model Type:",
                     choices = c("Pan Trap", "Sweep Net")
                   ),
                   uiOutput("variable_selector")
                 ),
                 column(9,
                      div(id = "plot_container",
                        style = "position: relative; height: 700px; width: 100%;",
                        plotly::plotlyOutput("main_plot", height = "700px"),
                        div(id = "png_container",
                          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                          uiOutput("png_wrapper")
                        )
                      )
                 )
               )
      ),
      tabPanel("Diagnostics",
               fluidRow(
                 column(3,
                   selectInput(
                     "model_type_diag",
                     "Select Model Type:",
                     choices = c("Pan Trap", "Sweep Net")
                   )
                 ),
                 column(9,
                   uiOutput("diagnostics_plot")
                 )
               )
      )
    )
  )
)
